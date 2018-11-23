// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <utility>

#include "vm/hash_map.h"
#include "vm/object.h"
#include "vm/raw_object.h"

namespace dart {

#if defined(DART_PRECOMPILER)

class OffsetsTable : public ZoneAllocated {
 public:
  OffsetsTable(Zone *zone);

  // Returns 'nullptr' if no offset was found.
  // Otherwise, the returned string is allocated in global static memory.
  const char* FieldNameForOffset(
      const char* class_name,
      intptr_t offset);

 private:
  struct OffsetsTableEntry {
    const char* class_name;
    const char* field_name;
    intptr_t offset;
  };

  static OffsetsTableEntry offsets_table[];

  struct StringAndIntToStringMapTraits {
    typedef std::pair<const char*, intptr_t> Key;
    typedef const char* Value;

    struct Pair {
      Key key;
      Value value;
      Pair() : key({nullptr, -1}), value(nullptr) {}
      Pair(Key k, Value v) : key(k), value(v) {}
    };

    static Value ValueOf(Pair pair) { return pair.value; }
    static Key KeyOf(Pair pair) { return pair.key; }
    static size_t Hashcode(Key key) {
      return String::Hash(key.first, strlen(key.first)) ^ key.second;
    }
    static bool IsKeyEqual(Pair x, Key y) {
      return strcmp(x.key.first, y.first) == 0 && x.key.second == y.second;
    }
  };

  DirectChainedHashMap<StringAndIntToStringMapTraits> cached_offsets_;
};

#else

class OffsetsTable : public ZoneAllocated {
 public:
  OffsetsTable(Zone* zone) {}

  const char* FieldNameForOffset(const char* class_name, intptr_t offset) {
    return nullptr;
  }
};

#endif

}  // namespace dart
