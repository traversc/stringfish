#ifndef SMALL_ARRAY_H
#define SMALL_ARRAY_H

#include <memory>
#include <algorithm>
#include <type_traits>
#include <cstdlib>
#include <cstring>

namespace trqwe {

// simple wrapper around a C style array with small array optimization
// similar to boost::small_vector but does not store capacity, no push_back method. 
// Can be re-allocated with reset(size) or resize(size)
template<class T, class Allocator = std::allocator<T>, class S = size_t, class stack_size = std::integral_constant<S, 32>>
class small_array {
  static_assert(std::is_pod<T>::value, "small_array type should be POD");
  static_assert(std::is_empty<Allocator>::value, "small_array requires a stateless allocator");
public:
  typedef T         value_type;
  typedef T*        pointer_type;
  typedef T const * const_pointer_type;
  typedef T&        reference_type;
  typedef T const & const_reference_type;
  typedef S         size_type;
private:
  struct Members : Allocator { // derive from Allocator to use empty base optimization
    value_type      _stack[stack_size::value] = {0}; // zero initialize
    pointer_type    _data;
    size_type       _size;
#if __cplusplus >= 201402L
    constexpr pointer_type stack_address() {
#else
    inline pointer_type stack_address() {
#endif
      return _stack;
    }
    inline pointer_type allocate_check_and_copy(value_type const * const data, const size_type size) {
      pointer_type result = allocate_check(size);
      if(size != 0) {
        std::copy(data, data + size, result);
      }
      return result;
    }
    inline pointer_type allocate_check(const size_type size) {
      if(size <= stack_size::value) {
        return stack_address();
      } else {
        return this->allocate(size);
      }
    }
    inline void deallocate_check() {
      deallocate_check(_data, _size);
    }
    inline void deallocate_check(pointer_type data, const size_type size) {
      if(size > stack_size::value) {
        this->deallocate(data, size);
      }
    }
    Members(value_type const * const data, const size_type size) : _data(allocate_check_and_copy(data, size)), _size(size) {}
    Members(const size_type size) : _data(allocate_check(size)), _size(size) {}
    Members() : _data(stack_address()), _size(0) {}
  } m;
  friend void swap(small_array & first, small_array & second) noexcept {
    using std::swap;
    std::swap(first.m._data, second.m._data);
    std::swap(first.m._size, second.m._size);
    std::swap(first.m._stack, second.m._stack);
    if(first.m._size <= stack_size::value) { first.m._data = first.m.stack_address(); }
    if(second.m._size <= stack_size::value) { second.m._data = second.m.stack_address(); }
  }
  
public:
  small_array() : m() {}
  small_array(value_type const * const data, const size_type size) : m(data, size) {}
  small_array(const size_type size) : m(size) {}
  small_array(const size_type size, const value_type value) : m(size) {
    if(size != 0) {
      std::fill(begin(), end(), value);
    }
  }
  // Copy
  small_array(small_array const & other) : m(other.m._data, other.m._size) {}
  // Move
  small_array(small_array && other) noexcept : m() { swap(*this, other); }
  // "copy and swap" covers both move and copy assignment https://stackoverflow.com/q/3279543/2723734
  small_array & operator=(small_array other) {
    swap(*this, other);
    return *this;
  }
  // destructor
  ~small_array() { m.deallocate_check(); }
  inline bool is_stack() const { return m._size <= stack_size::value; }
  void reset(const size_type size) {
    pointer_type new_data = m.allocate_check(size);
    pointer_type old_data = m._data;
    size_type old_size = m._size;
    m._data = new_data;
    m._size = size;
    m.deallocate_check(old_data, old_size);
  }
  void resize(const size_type size) {
    const size_type copy_size = std::min(size, m._size);
    if(is_stack()) {
      pointer_type new_addr = m.allocate_check(size);
      if(copy_size != 0) {
        std::memmove(new_addr, m._data, copy_size * sizeof(value_type)); // memmove if new size is also stack
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
  const_pointer_type data() const { return m._data; }
  pointer_type data() { return m._data; }
  const_reference_type operator[](size_type idx) const { return *(m._data + idx); }
  reference_type operator[](size_type idx) { return *(m._data + idx); }
  const_pointer_type begin() const { return m._data; }
  const_pointer_type end() const { return m._data + m._size; }
  pointer_type begin() { return m._data; }
  pointer_type end() { return m._data + m._size; }
};

}

#endif // include guard
