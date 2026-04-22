#ifndef SMALL_NULLABLE_ARRAY_H
#define SMALL_NULLABLE_ARRAY_H

#include <memory>
#include <algorithm>
#include <type_traits>
#include <cstdlib>
#include <cstring>
#include <limits>

namespace trqwe {

// simple wrapper around a C style array with small array optimization. Nullable.
// similar to boost::small_vector but does not store capacity, no push_back method. 
// Can be re-allocated with reset(size) or resize(size)
// Check null status or make null: is_null and nullify
template<class T, class Allocator = std::allocator<T>, class S = size_t, class stack_size = std::integral_constant<S, 8>>
class small_nullable_array {
  static_assert(std::is_pod<T>::value, "small_nullable_array type should be POD");
  static_assert(std::is_empty<Allocator>::value, "small_nullable_array requires a stateless allocator");
public:
  typedef T         value_type;
  typedef T*        pointer_type;
  typedef T const * const_pointer_type;
  typedef T&        reference_type;
  typedef T const & const_reference_type;
  typedef S         size_type;
  static constexpr size_type nullsize = std::numeric_limits<size_type>::max();
private:
  struct Members : Allocator { // derive from Allocator to use empty base optimization
    value_type            _stack[stack_size::value] = {0}; // zero initialize
    pointer_type          _data;
    size_type             _size;
#if __cplusplus >= 201402L
    constexpr pointer_type stack_address() {
#else
    inline pointer_type stack_address() {
#endif
      return _stack;
    }
    inline pointer_type allocate_check_and_copy(value_type const * const data, const size_type size) {
      if((size == nullsize) || (size == 0)) {
        return stack_address();
      } else if(size <= stack_size::value) {
        pointer_type result = stack_address();
        std::copy(data, data + size, result);
        return result;
      } else {
        pointer_type result = this->allocate(size);
        std::copy(data, data + size, result);
        return result;
      }
    }
    inline pointer_type allocate_check(const size_type size) {
      if((size == nullsize) || (size <= stack_size::value)) {
        return stack_address();
      } else {
        return this->allocate(size);
      }
    }
    inline void deallocate_check() {
      deallocate_check(_data, _size);
    }
    inline void deallocate_check(pointer_type data, const size_type size) {
      if((size > stack_size::value) && (size != nullsize)) {
        this->deallocate(data, size);
      }
    }
    inline bool uses_stack() const {
      return (_size <= stack_size::value) || (_size == nullsize);
    }
    inline void refresh_data_pointer() {
      if(uses_stack()) {
        _data = stack_address();
      }
    }
    Members(value_type const * const data, const size_type size) : _data(allocate_check_and_copy(data, size)), _size(size) {}
    Members(const size_type size) : _data(allocate_check(size)), _size(size) {}
    Members() : _data(stack_address()), _size(0) {}
  } m;
  friend void swap(small_nullable_array & first, small_nullable_array & second) noexcept {
    using std::swap;
    std::swap(first.m._data, second.m._data);
    std::swap(first.m._size, second.m._size);
    std::swap(first.m._stack, second.m._stack);
    first.m.refresh_data_pointer();
    second.m.refresh_data_pointer();
  }
  
public:
  small_nullable_array() : m() {}
  small_nullable_array(value_type const * const data, const size_type size) : m(data, size) {}
  small_nullable_array(const size_type size) : m(size) {}
  small_nullable_array(const size_type size, const value_type value) : m(size) {
    if((size != 0) && (size != nullsize)) {
      std::fill(begin(), end(), value);
    }
  }
  // Copy
  small_nullable_array(small_nullable_array const & other) : m(other.m._data, other.m._size) {}
  // Move
  small_nullable_array(small_nullable_array && other) noexcept : m() { swap(*this, other); }
  // "copy and swap" covers both move and copy assignment https://stackoverflow.com/q/3279543/2723734
  small_nullable_array & operator=(small_nullable_array other) {
    swap(*this, other);
    return *this;
  }
  // destructor
  ~small_nullable_array() { m.deallocate_check(); }
  inline bool is_stack() const { return (m._size <= stack_size::value) || (m._size == nullsize); }
  inline bool is_null() const { return m._size == nullsize; }
  void nullify() {
    m.deallocate_check();
    m._data = m.stack_address();
    m._size = nullsize;
  }
  // reset and resize are the same, but reset doesn't copy over old data
  // we don't have m._capacity so there is always a re-allocation
  void reset(const size_type size) {
    if(size == nullsize) {
      nullify();
      return;
    }
    pointer_type new_data = m.allocate_check(size);
    pointer_type old_data = m._data;
    size_type old_size = m._size;
    m._data = new_data;
    m._size = size;
    m.deallocate_check(old_data, old_size);
  }
  void resize(const size_type size) {
    if(size == nullsize) {
      nullify();
      return;
    }
    const size_type copy_size = std::min(size, m._size);
    if(is_null()) {
      m._data = m.allocate_check(size);
      m._size = size;
    } else if(is_stack()) {
      pointer_type new_addr = m.allocate_check(size);
      if(copy_size != 0) {
        std::memmove(new_addr, m._data, copy_size * sizeof(value_type)); // need memmove if new size is also stack allocated
      }
      m._data = new_addr;
      m._size = size;
    } else {
      pointer_type new_addr = m.allocate_check(size);
      if(copy_size != 0) {
        std::copy(m._data, m._data + copy_size, new_addr);
      }
      pointer_type old_data = m._data;
      size_type old_size = m._size;
      m._data = new_addr;
      m._size = size;
      m.deallocate_check(old_data, old_size);
    }
  }
  size_type size() const { return m._size; }
  const_pointer_type data() const { return is_null() ? nullptr : m._data; }
  pointer_type data() { return is_null() ? nullptr : m._data; }
  const_reference_type operator[](size_type idx) const { return *(m._data + idx); }
  reference_type operator[](size_type idx) { return *(m._data + idx); }
  const_pointer_type begin() const { return data(); }
  const_pointer_type end() const { return is_null() ? nullptr : m._data + m._size; }
  pointer_type begin() { return data(); }
  pointer_type end() { return is_null() ? nullptr : m._data + m._size; }
};

} // end namespace

#endif // include guard
