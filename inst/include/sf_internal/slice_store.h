#ifndef STRINGFISH_SF_INTERNAL_SLICE_STORE_H
#define STRINGFISH_SF_INTERNAL_SLICE_STORE_H

#include "string_types.h"
#include <optional>

// slice_store_data is the underlying data type for slice_store ALTREP class
// slice_store_shard is for the purposes of a multi-threaded constructor (caller defines the multi-threading framework)
//   slice_store_shard shares a records vector with other shards, but has its own slice store
//   After writing slices to each shard and filling the combined records vector, caller can combine shards with
//   constructor template<typename Shards> slice_store_data(std::vector<string_record> && source_records, Shards & shards)

// slice_store_data
//
// Storage and slice size growth
// We store memory in contiguous blocks ("slices") of data.
// The initial slice size depends on the initialized vector length.
// For vectors of length 0 or 1 we defer allocation until reserve()/assign() actually needs bytes.
// For vectors of length >= 2 we keep a small pooled initial slice.
// Formula:
//   len <= 1  -> 0
//   len >= 2  -> min(max(64, next_power_of_two(len * 4)), max_initial_slice_bytes)
// The slice_store_data(size_t, size_t) overload overrides this first-slice heuristic.
//
// Subsequent tail slices grow from that baseline using allocated_bytes / 2 as a conservative hint.
// If a single string is larger than the regular growth target, the next tail is at least as large as the string.
// Formula: next_tail_size = max(next_regular_slice_size(), len)
//   and then rounded up to nearest slice_alignment and then capped at max_slice_bytes.

// String mutation
// Each string can be mutated and replaced with a new string with the following accessor methods
// char * reserve(size_t idx, size_t len, cetype_t_ext enc)
// void assign(size_t idx, const char * ptr, size_t len, cetype_t_ext enc)
// void assign(size_t idx, const sfstring & value)
// If the new string size is <= old string size, mutation happens in place
// If the new string size is larger then it is given a new record location in the tail slice or
//   If the remaining bytes in the tail is not large enough a new tail is appended that is at least as large as the new string
//   Then dead_bytes is incremented to account for the unused space at the old record location

// String compaction
// If dead_bytes >= compact_dead_threshold AND dead_bytes >= (live_bytes + 1) / 2, where live_bytes = allocated_bytes - dead_bytes
// We do a compaction to reduce dead_bytes to zero and in order for the session to reclaim memory
// We create compacted slices each with size <= UINT32_MAX and memcpy record data into these new compacted slices
// Compacted slices are exact-fit for the live payload placed in each slice. Bookkeeping values are updated as follows:
// dead_bytes <- 0
// allocate <- sum(len for each records)
//   The final compacted tail is marked full, so a new reserve() or assign() call creates a fresh tail block

struct slice_store_shard {
  static constexpr size_t min_multi_string_slice_bytes = 64;
  static constexpr size_t max_initial_slice_bytes = 16U << 10; // 16 KiB
  static constexpr size_t max_slice_bytes = 256U << 10; // 256 KiB
  static constexpr size_t slice_alignment = 64;
  static constexpr size_t vector_len_scale = 4;

  std::vector<std::unique_ptr<char[]>> slices;
  std::vector<string_record> * records;
  size_t allocated_bytes;
  uint32_t current_slice_used;
  uint32_t current_slice_capacity;

  // pointer address for 0-len strings
  static const char * empty_data() noexcept {
    static const char empty = '\0';
    return &empty;
  }

  size_t initial_slice_size() const noexcept {
    const size_t vector_len = records == nullptr ? 0 : records->size();
    if(vector_len <= 1) {
      return 0;
    }
    const size_t scaled = sfcalc::next_power_of_two(vector_len * vector_len_scale);
    return std::min(std::max(min_multi_string_slice_bytes, scaled), max_initial_slice_bytes);
  }

  size_t next_regular_slice_size() const noexcept {
    const size_t initial = initial_slice_size();
    const size_t grown = sfcalc::round_up(std::max(initial, allocated_bytes / 2), slice_alignment);
    return std::min(grown, max_slice_bytes);
  }

  void allocate_slice(uint32_t capacity) {
    slices.emplace_back(new char[capacity]);
    current_slice_used = 0;
    current_slice_capacity = capacity;
  }

  char * allocate_bytes(uint32_t len) {
    if(len == 0) {
      return const_cast<char*>(empty_data());
    }
    if(slices.empty() || current_slice_capacity - current_slice_used < len) {
      const size_t next_size = sfcalc::round_up(std::max(next_regular_slice_size(), static_cast<size_t>(len)), slice_alignment);
      allocate_slice(checked_sf_size(next_size, "slice size"));
    }
    char * dest = slices.back().get() + current_slice_used;
    current_slice_used += len;
    allocated_bytes += static_cast<size_t>(len);
    return dest;
  }

  slice_store_shard() :
    slices(),
    records(nullptr),
    allocated_bytes(0),
    current_slice_used(0),
    current_slice_capacity(0) {}

  explicit slice_store_shard(std::vector<string_record> & records) :
    slices(),
    records(&records),
    allocated_bytes(0),
    current_slice_used(0),
    current_slice_capacity(0) {}

  slice_store_shard(const slice_store_shard &) = delete;
  slice_store_shard & operator=(const slice_store_shard &) = delete;
  slice_store_shard(slice_store_shard &&) noexcept = default;
  slice_store_shard & operator=(slice_store_shard &&) noexcept = default;

  char * reserve(size_t idx, size_t len, cetype_t_ext enc) {
    if(records == nullptr) {
      throw std::runtime_error("slice_store_shard has no records");
    }
    if(idx >= records->size()) {
      throw std::runtime_error("slice_store_shard assignment out of bounds");
    }
    if(!check_r_string_len(len)) {
      throw std::runtime_error("stored string length exceeds R string size");
    }
    const uint32_t stored_len = static_cast<uint32_t>(len);
    if(enc == cetype_t_ext::CE_NA) {
      (*records)[idx] = string_record{nullptr, 0, cetype_t_ext::CE_NA};
      return nullptr;
    }
    if(stored_len == 0) {
      (*records)[idx] = string_record{empty_data(), 0, enc};
      return const_cast<char*>(empty_data());
    }
    char * dest = allocate_bytes(stored_len);
    (*records)[idx] = string_record{dest, stored_len, enc};
    return dest;
  }

  void assign(size_t idx, const char * ptr, size_t len, cetype_t_ext enc) {
    if(enc != cetype_t_ext::CE_NA && ptr == nullptr && len > 0) {
      throw std::runtime_error("cannot assign non-NA null bytes");
    }
    char * dest = reserve(idx, len, enc);
    if(dest != nullptr && len > 0) {
      std::memcpy(dest, ptr, len);
    }
  }

  void assign(size_t idx, const sfstring & value) {
    assign(idx, value.data(), value.size(), value.encoding);
  }

  void assign(size_t idx, const char * ptr, size_t len, cetype_t enc) {
    assign(idx, ptr, len, reinterpret_input_encoding(ptr, len, enc));
  }
};

struct slice_store_data {
  static constexpr size_t min_multi_string_slice_bytes = 64;
  static constexpr size_t max_initial_slice_bytes = 16U << 10; // 16 KiB
  static constexpr size_t max_slice_bytes = 256U << 10; // 256 KiB
  static constexpr size_t compact_dead_threshold = 1U << 20;
  static constexpr size_t slice_alignment = 64;
  static constexpr size_t vector_len_scale = 4;

  class reference {
  private:
    slice_store_data * owner;
    size_t idx;
  public:
    reference(slice_store_data * owner, size_t idx) : owner(owner), idx(idx) {}
    reference & operator=(const sfstring & value) {
      owner->assign(idx, value);
      return *this;
    }
    reference & operator=(sfstring && value) {
      owner->assign(idx, value);
      return *this;
    }
    operator sfstring() const {
      const auto & rec = owner->records[idx];
      if(rec.is_NA()) {
        return sfstring(NA_STRING);
      }
      if(!check_r_string_len(rec.len)) {
        throw std::runtime_error("string size exceeds R string size");
      }
      return sfstring(rec.ptr, static_cast<int>(rec.len), rec.encoding);
    }
  };

  std::vector<std::unique_ptr<char[]>> slices;
  std::vector<string_record> records;
  size_t allocated_bytes;
  size_t dead_bytes;
  uint32_t current_slice_used;
  uint32_t current_slice_capacity;
  std::optional<size_t> initial_slice_size_override;

  static const char * empty_data() noexcept {
    static const char empty = '\0';
    return &empty;
  }

  static string_record na_record() noexcept {
    return string_record{nullptr, 0, cetype_t_ext::CE_NA};
  }

  static string_record empty_record(cetype_t_ext enc) noexcept {
    return string_record{empty_data(), 0, enc};
  }

  static size_t normalize_initial_slice_size(size_t value) {
    if(value == 0) {
      return 0;
    }
    return static_cast<size_t>(
      checked_sf_size(sfcalc::round_up(value, slice_alignment), "initial slice size")
    );
  }

  size_t heuristic_initial_slice_size() const noexcept {
    const size_t vector_len = records.size();
    if(vector_len <= 1) {
      return 0;
    }
    const size_t scaled = sfcalc::next_power_of_two(vector_len * vector_len_scale);
    return std::min(std::max(min_multi_string_slice_bytes, scaled), max_initial_slice_bytes);
  }

  size_t initial_slice_size() const noexcept {
    if(initial_slice_size_override.has_value()) {
      return *initial_slice_size_override;
    }
    return heuristic_initial_slice_size();
  }

  size_t next_regular_slice_size() const noexcept {
    const size_t initial = heuristic_initial_slice_size();
    const size_t grown = sfcalc::round_up(std::max(initial, allocated_bytes / 2), slice_alignment);
    return std::min(grown, max_slice_bytes);
  }

  void allocate_slice(uint32_t capacity) {
    slices.emplace_back(new char[capacity]);
    current_slice_used = 0;
    current_slice_capacity = capacity;
  }

  char * allocate_bytes(uint32_t len) {
    if(len == 0) {
      return const_cast<char*>(empty_data());
    }
    if(slices.empty()) {
      const size_t next_size = sfcalc::round_up(
        std::max(initial_slice_size(), static_cast<size_t>(len)),
        slice_alignment
      );
      allocate_slice(checked_sf_size(next_size, "slice size"));
    } else if(current_slice_capacity - current_slice_used < len) {
      const size_t next_size = sfcalc::round_up(std::max(next_regular_slice_size(), static_cast<size_t>(len)), slice_alignment);
      allocate_slice(checked_sf_size(next_size, "slice size"));
    }
    char * dest = slices.back().get() + current_slice_used;
    current_slice_used += len;
    allocated_bytes += static_cast<size_t>(len);
    return dest;
  }

  inline bool should_compact() const noexcept {
    if(dead_bytes < compact_dead_threshold) {
      return false;
    }
    if(allocated_bytes <= dead_bytes) {
      return false;
    }
    const size_t live = allocated_bytes - dead_bytes;
    return dead_bytes >= ((live + 1) / 2);
  }

  slice_store_data() :
    slices(),
    records(),
    allocated_bytes(0),
    dead_bytes(0),
    current_slice_used(0),
    current_slice_capacity(0),
    initial_slice_size_override(std::nullopt) {}

  explicit slice_store_data(size_t len) : slice_store_data() {
    records.resize(len, na_record());
  }

  slice_store_data(size_t len, size_t initial_slice_size) : slice_store_data() {
    records.resize(len, na_record());
    initial_slice_size_override = normalize_initial_slice_size(initial_slice_size);
    if(*initial_slice_size_override > 0) {
      allocate_slice(static_cast<uint32_t>(*initial_slice_size_override));
    }
  }

  slice_store_data(const slice_store_data & other) : slice_store_data() {
    initial_slice_size_override = other.initial_slice_size_override;
    records.resize(other.records.size(), na_record());
    for(size_t i = 0; i < other.records.size(); ++i) {
      const auto & rec = other.records[i];
      assign_trusted(i, rec.ptr, static_cast<size_t>(rec.len), rec.encoding);
    }
  }

  template<typename Shards>
  slice_store_data(std::vector<string_record> && source_records, Shards & shards) :
    slices(),
    records(std::move(source_records)),
    allocated_bytes(0),
    dead_bytes(0),
    current_slice_used(0),
    current_slice_capacity(0),
    initial_slice_size_override(std::nullopt) {
    bool have_best = false;
    uint32_t best_remaining = 0;
    const void * best_shard = nullptr;
    for(auto & shard : shards) {
      if(shard.slices.empty()) {
        continue;
      }
      const uint32_t remaining = shard.current_slice_capacity - shard.current_slice_used;
      if(!have_best || remaining > best_remaining) {
        have_best = true;
        best_remaining = remaining;
        best_shard = static_cast<const void*>(&shard);
      }
    }

    auto append_shard = [&](auto & shard) {
      if(shard.slices.empty()) {
        return;
      }
      allocated_bytes += shard.allocated_bytes;
      for(auto & slice : shard.slices) {
        slices.push_back(std::move(slice));
      }
      current_slice_used = shard.current_slice_used;
      current_slice_capacity = shard.current_slice_capacity;
    };

    for(auto & shard : shards) {
      if(have_best && static_cast<const void*>(&shard) == best_shard) {
        continue;
      }
      append_shard(shard);
    }
    if(have_best) {
      for(auto & shard : shards) {
        if(static_cast<const void*>(&shard) == best_shard) {
          append_shard(shard);
          break;
        }
      }
    } else {
      current_slice_used = 0;
      current_slice_capacity = 0;
    }
  }

  slice_store_data(slice_store_data &&) noexcept = default;

  slice_store_data & operator=(const slice_store_data & other) {
    if(this == &other) {
      return *this;
    }
    slice_store_data tmp(other);
    *this = std::move(tmp);
    return *this;
  }

  slice_store_data & operator=(slice_store_data &&) noexcept = default;

  void reserve(size_t n) {
    records.reserve(n);
  }

  void resize(size_t n) {
    records.resize(n, na_record());
  }

  void clear() {
    slices.clear();
    records.clear();
    allocated_bytes = 0;
    dead_bytes = 0;
    current_slice_used = 0;
    current_slice_capacity = 0;
  }

  inline reference operator[](size_t idx) {
    return reference(this, idx);
  }

  inline sfstring operator[](size_t idx) const {
    const auto & rec = records[idx];
    if(rec.is_NA()) {
      return sfstring(NA_STRING);
    }
    if(!check_r_string_len(rec.len)) {
      throw std::runtime_error("string size exceeds R string size");
    }
    return sfstring(rec.ptr, static_cast<int>(rec.len), rec.encoding);
  }

  void compact() {
    if(dead_bytes == 0) {
      return;
    }
    auto reset_after_compaction_failure = [&]() {
      for(auto & rec : records) {
        if(rec.is_NA()) {
          rec = na_record();
        } else if(rec.len == 0) {
          rec = empty_record(rec.encoding);
        } else {
          rec = na_record();
        }
      }
      slices.clear();
      allocated_bytes = 0;
      dead_bytes = 0;
      current_slice_used = 0;
      current_slice_capacity = 0;
    };
    if(allocated_bytes < dead_bytes) {
      REprintf("stringfish: slice_store_data compact() saw dead_bytes > allocated_bytes; resetting store\n");
      reset_after_compaction_failure();
      return;
    }
    if(allocated_bytes == dead_bytes) {
      reset_after_compaction_failure();
      return;
    }
    const size_t live_bytes = allocated_bytes - dead_bytes;

    std::vector<string_record> compacted_records(records);
    std::vector<std::unique_ptr<char[]>> compacted_slices;
    uint32_t last_block_size = 0;
    const size_t max_block_bytes = static_cast<size_t>(std::numeric_limits<uint32_t>::max());

    auto emit_block = [&](size_t start, size_t end, size_t block_size) {
      if(block_size == 0) {
        return;
      }
      uint32_t compacted_size = checked_sf_size(block_size, "compacted slice size");
      std::unique_ptr<char[]> slice(new char[compacted_size]);
      char * write_ptr = slice.get();
      char * block_start = write_ptr;
      for(size_t i = start; i < end; ++i) {
        const auto & rec = records[i];
        if(rec.is_NA() || rec.len == 0) {
          continue;
        }
        std::memcpy(write_ptr, rec.ptr, static_cast<size_t>(rec.len));
        compacted_records[i] = string_record{write_ptr, rec.len, rec.encoding};
        write_ptr += rec.len;
      }
      if(static_cast<size_t>(write_ptr - block_start) != block_size) {
        throw std::runtime_error("slice_store_data compact size mismatch");
      }
      compacted_slices.push_back(std::move(slice));
      last_block_size = compacted_size;
    };

    size_t block_start = 0;
    size_t block_end = 0;
    size_t block_size = 0;
    while(block_end < records.size()) {
      const auto & rec = records[block_end];
      const size_t next_size = rec.is_NA() ? 0 : static_cast<size_t>(rec.len);
      if(next_size > max_block_bytes - block_size) {
        emit_block(block_start, block_end, block_size);
        block_start = block_end;
        block_size = 0;
        continue;
      }
      block_size += next_size;
      ++block_end;
    }
    emit_block(block_start, block_end, block_size);

    slices = std::move(compacted_slices);
    records = std::move(compacted_records);
    allocated_bytes = live_bytes;
    dead_bytes = 0;
    if(last_block_size == 0) {
      current_slice_used = 0;
      current_slice_capacity = 0;
    } else {
      current_slice_used = last_block_size;
      current_slice_capacity = last_block_size;
    }
  }

  void append_trusted(const char * ptr, size_t len, cetype_t_ext enc) {
    if(!check_r_string_len(len)) {
      throw std::runtime_error("stored string length exceeds R string size");
    }
    const uint32_t stored_len = static_cast<uint32_t>(len);
    if(enc == cetype_t_ext::CE_NA || ptr == nullptr) {
      records.emplace_back(na_record());
      return;
    }
    if(stored_len == 0) {
      records.emplace_back(empty_record(enc));
      return;
    }
    char * dest = allocate_bytes(stored_len);
    std::memcpy(dest, ptr, static_cast<size_t>(stored_len));
    records.emplace_back(string_record{dest, stored_len, enc});
  }

  void append_trusted(const sfstring & value) {
    append_trusted(value.data(), value.size(), value.encoding);
  }

  void push_back(const sfstring & value) {
    append_trusted(value);
  }

  void push_back(sfstring && value) {
    append_trusted(value);
  }

  template<typename... Args>
  void emplace_back(Args&&... args) {
    push_back(sfstring(std::forward<Args>(args)...));
  }

  char * reserve(size_t idx, size_t len, cetype_t_ext enc) {
    if(!check_r_string_len(len)) {
      throw std::runtime_error("stored string length exceeds R string size");
    }
    const uint32_t stored_len = static_cast<uint32_t>(len);
    if(idx >= records.size()) {
      throw std::runtime_error("slice_store_data assignment out of bounds");
    }

    string_record current = records[idx];
    if(enc == cetype_t_ext::CE_NA) {
      records[idx] = na_record();
      return nullptr;
    }
    if(stored_len == 0) {
      records[idx] = empty_record(enc);
      return const_cast<char*>(empty_data());
    }
    if(!current.is_NA()) {
      if(current.len >= stored_len) {
        records[idx] = string_record{current.ptr, stored_len, enc};
        return const_cast<char*>(current.ptr);
      }
      if(should_compact()) {
        compact();
        current = records[idx];
      }
      dead_bytes += static_cast<size_t>(current.len);
    }
    char * dest = allocate_bytes(stored_len);
    records[idx] = string_record{dest, stored_len, enc};
    return dest;
  }

  void assign_trusted(size_t idx, const char * ptr, size_t len, cetype_t_ext enc) {
    if(enc != cetype_t_ext::CE_NA && ptr == nullptr && len > 0) {
      throw std::runtime_error("cannot assign non-NA null bytes");
    }
    char * dest = reserve(idx, len, enc);
    if(dest != nullptr && len > 0) {
      std::memcpy(dest, ptr, len);
    }
  }

  void assign(size_t idx, const sfstring & value) {
    assign_trusted(idx, value.data(), value.size(), value.encoding);
  }

  void assign(size_t idx, const char * ptr, size_t len, cetype_t_ext enc) {
    assign_trusted(idx, ptr, len, enc);
  }

  void assign(size_t idx, const char * ptr, size_t len, cetype_t enc) {
    assign_trusted(idx, ptr, len, reinterpret_input_encoding(ptr, len, enc));
  }
};

#endif
