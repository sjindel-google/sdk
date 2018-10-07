// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

/// Declares the type system used by global type flow analysis.
library vm.transformations.type_flow.types;

import 'dart:core' hide Type;

import 'package:kernel/ast.dart';

import 'utils.dart';

abstract class GenericInterfacesInfo {
  List<DartType> flattenedTypeArgumentsFor(Class klass);
  int genericInterfaceOffsetFor(Class klass, Class iface);
}

/// Abstract interface to type hierarchy information used by types.
abstract class TypeHierarchy implements GenericInterfacesInfo {
  /// Test if [subType] is a subtype of [superType].
  bool isSubtype(DartType subType, DartType superType);

  /// Return a more specific type for the type cone with [base] root.
  /// May return EmptyType, AnyType, ConcreteType or a SetType.
  Type specializeTypeCone(DartType base);
}

/// Basic normalization of Dart types.
/// Currently used to approximate generic and function types.
DartType _normalizeDartType(DartType type) {
  if (type is InterfaceType) {
    // TODO(alexmarkov): take generic type arguments into account
    return type.classNode.rawType;
  } else if (type is FunctionType) {
    // TODO(alexmarkov): support function types
    return const DynamicType();
  } else if (type is TypeParameterType) {
    // TODO(alexmarkov): instantiate type parameters if possible
    final bound = type.bound;
    // Protect against infinite recursion in case of cyclic type parameters
    // like 'T extends T'. As of today, front-end doesn't report errors in such
    // cases yet.
    if (bound is TypeParameterType) {
      return const DynamicType();
    }
    return _normalizeDartType(bound);
  }
  return type;
}

/// Base class for type expressions.
/// Type expression is either a [Type] or a statement in a summary.
abstract class TypeExpr {
  const TypeExpr();

  /// Returns computed type of this type expression.
  /// [types] is the list of types computed for the statements in the summary.
  Type getComputedType(List<Type> types);
}

/// Base class for types inferred by the type flow analysis.
/// [Type] describes a specific set of values (Dart instances) and does not
/// directly correspond to a Dart type.
/// TODO(alexmarkov): consider detaching Type hierarchy from TypeExpr/Statement.
abstract class Type extends TypeExpr {
  const Type();

  /// Create an empty type.
  factory Type.empty() => const EmptyType();

  /// Create a non-nullable type representing a subtype cone. It contains
  /// instances of all Dart types which extend, mix-in or implement [dartType].
  factory Type.cone(DartType dartType) {
    dartType = _normalizeDartType(dartType);
    if ((dartType == const DynamicType()) || (dartType == const VoidType())) {
      return const AnyType();
    } else {
      return new ConeType(dartType);
    }
  }

  /// Create a nullable type - union of [t] and the `null` object.
  factory Type.nullable(Type t) => new NullableType(t);

  /// Create a type representing arbitrary nullable object (`dynamic`).
  factory Type.nullableAny() => new NullableType(const AnyType());

  /// Create a Type which corresponds to a set of instances constrained by
  /// Dart type annotation [dartType].
  factory Type.fromStatic(DartType dartType) {
    dartType = _normalizeDartType(dartType);
    if ((dartType == const DynamicType()) || (dartType == const VoidType())) {
      return new Type.nullableAny();
    } else if (dartType == const BottomType()) {
      return new Type.nullable(new Type.empty());
    }
    return new Type.nullable(new ConeType(dartType));
  }

  Class getConcreteClass(TypeHierarchy typeHierarchy) => null;

  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) => false;

  // Returns 'true' if this type will definitely pass a runtime type-check
  // against 'runtimeType'. Returns 'false' if the test might fail (e.g. due to
  // an approximation).
  bool isDefinitelySubtypeOfRuntimeType(
      TypeHierarchy typeHierarchy, RuntimeType runtimeType);

  @override
  Type getComputedType(List<Type> types) => this;

  /// Order of precedence for evaluation of union/intersection.
  int get order;

  /// Returns true iff this type is fully specialized.
  bool get isSpecialized => true;

  /// Returns specialization of this type using the given [TypeHierarchy].
  Type specialize(TypeHierarchy typeHierarchy) => this;

  /// Calculate union of this and [other] types.
  Type union(Type other, TypeHierarchy typeHierarchy);

  /// Calculate intersection of this and [other] types.
  Type intersection(Type other, TypeHierarchy typeHierarchy);
}

/// Order of precedence between types for evaluation of union/intersection.
enum TypeOrder {
  RuntimeType,
  Empty,
  Nullable,
  Any,
  Set,
  Cone,
  Concrete,
}

/// Type representing the empty set of instances.
class EmptyType extends Type {
  const EmptyType();

  @override
  int get hashCode => 997;

  @override
  bool operator ==(other) => (other is EmptyType);

  @override
  String toString() => "_T {}";

  @override
  int get order => TypeOrder.Empty.index;

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) => other;

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) => this;

  bool isDefinitelySubtypeOfRuntimeType(
          TypeHierarchy typeHierarchy, RuntimeType other) =>
      true;
}

/// Nullable type represents a union of a (non-nullable) type and the `null`
/// object. Other kinds of types do not contain `null` object (even AnyType).
class NullableType extends Type {
  final Type baseType;

  NullableType(this.baseType) {
    assertx(baseType != null);
    assertx(baseType is! NullableType);
  }

  @override
  int get hashCode => (baseType.hashCode + 31) & kHashMask;

  @override
  bool operator ==(other) =>
      (other is NullableType) && (this.baseType == other.baseType);

  @override
  String toString() => "${baseType}?";

  @override
  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) =>
      baseType.isSubtypeOf(typeHierarchy, dartType);

  bool isDefinitelySubtypeOfRuntimeType(
          TypeHierarchy typeHierarchy, RuntimeType other) =>
      baseType.isDefinitelySubtypeOfRuntimeType(typeHierarchy, other);

  @override
  int get order => TypeOrder.Nullable.index;

  @override
  bool get isSpecialized => baseType.isSpecialized;

  @override
  Type specialize(TypeHierarchy typeHierarchy) => baseType.isSpecialized
      ? this
      : new NullableType(baseType.specialize(typeHierarchy));

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.union(this, typeHierarchy);
    }
    if (other is NullableType) {
      return new NullableType(baseType.union(other.baseType, typeHierarchy));
    } else {
      return new NullableType(baseType.union(other, typeHierarchy));
    }
  }

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.intersection(this, typeHierarchy);
    }
    if (other is NullableType) {
      return new NullableType(
          baseType.intersection(other.baseType, typeHierarchy));
    } else {
      return baseType.intersection(other, typeHierarchy);
    }
  }
}

/// Type representing any instance except `null`.
/// Semantically equivalent to ConeType of Object, but more efficient.
/// Can also represent a set of types, the set of all types.
class AnyType extends Type {
  const AnyType();

  @override
  int get hashCode => 991;

  @override
  bool operator ==(other) => (other is AnyType);

  @override
  String toString() => "_T ANY";

  @override
  int get order => TypeOrder.Any.index;

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.union(this, typeHierarchy);
    }
    return this;
  }

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.intersection(this, typeHierarchy);
    }
    return other;
  }

  bool isDefinitelySubtypeOfRuntimeType(
      TypeHierarchy typeHierarchy, RuntimeType other) {
    return other._type is DynamicType || other._type is VoidType;
  }
}

/// SetType is a union of concrete types T1, T2, ..., Tn, where n >= 2.
/// It represents the set of instances which types are in the {T1, T2, ..., Tn}.
class SetType extends Type {
  /// List of concrete types, sorted by classId.
  final List<ConcreteType> types;
  int _hashCode;

  /// Creates a new SetType using list of concrete types sorted by classId.
  SetType(this.types) {
    assertx(types.length >= 2);
    assertx(isSorted(types));
  }

  @override
  int get hashCode => _hashCode ??= _computeHashCode();

  int _computeHashCode() {
    int hash = 1237;
    for (var t in types) {
      hash = (((hash * 31) & kHashMask) + t.hashCode) & kHashMask;
    }
    return hash;
  }

  @override
  bool operator ==(other) {
    if ((other is SetType) && (types.length == other.types.length)) {
      for (int i = 0; i < types.length; i++) {
        if (types[i] != other.types[i]) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  @override
  String toString() => "_T ${types}";

  @override
  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) =>
      types.every((ConcreteType t) => t.isSubtypeOf(typeHierarchy, dartType));

  bool isDefinitelySubtypeOfRuntimeType(
          TypeHierarchy typeHierarchy, RuntimeType other) =>
      types.every(
          (t) => t.isDefinitelySubtypeOfRuntimeType(typeHierarchy, other));

  @override
  int get order => TypeOrder.Set.index;

  static List<ConcreteType> _unionLists(
      List<ConcreteType> types1, List<ConcreteType> types2) {
    int i1 = 0;
    int i2 = 0;
    List<ConcreteType> types = <ConcreteType>[];
    while ((i1 < types1.length) && (i2 < types2.length)) {
      final t1 = types1[i1];
      final t2 = types2[i2];
      final relation = t1.classId.compareTo(t2.classId);
      if (relation < 0) {
        types.add(t1);
        ++i1;
      } else if (relation > 0) {
        types.add(t2);
        ++i2;
      } else {
        if (t1 == t2) {
          types.add(t1);
        } else {
          // TODO(sjindel/tfa): Merge the type arguments vectors.
          // (e.g., Map<?, int> vs Map<String, int> can become Map<?, int>)
          types.add(t1.raw);
        }
        ++i1;
        ++i2;
      }
    }
    if (i1 < types1.length) {
      types.addAll(types1.getRange(i1, types1.length));
    } else if (i2 < types2.length) {
      types.addAll(types2.getRange(i2, types2.length));
    }
    return types;
  }

  static List<ConcreteType> _intersectLists(
      List<ConcreteType> types1, List<ConcreteType> types2) {
    int i1 = 0;
    int i2 = 0;
    List<ConcreteType> types = <ConcreteType>[];
    while ((i1 < types1.length) && (i2 < types2.length)) {
      final t1 = types1[i1];
      final t2 = types2[i2];
      final relation = t1.classId.compareTo(t2.classId);
      if (relation < 0) {
        ++i1;
      } else if (relation > 0) {
        ++i2;
      } else {
        if (t1.ct_typeArgs == null && t2.ct_typeArgs == null) {
          types.add(t1);
        } else {
          final intersect = t1.intersection(t2, null);
          if (intersect is! EmptyType) {
            types.add(intersect);
          }
        }
        ++i1;
        ++i2;
      }
    }
    return types;
  }

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.union(this, typeHierarchy);
    }
    if (other is SetType) {
      return new SetType(_unionLists(types, other.types));
    } else if (other is ConcreteType) {
      return types.contains(other)
          ? this
          : new SetType(_unionLists(types, <ConcreteType>[other]));
    } else if (other is ConeType) {
      return typeHierarchy
          .specializeTypeCone(other.dartType)
          .union(this, typeHierarchy);
    } else {
      throw 'Unexpected type $other';
    }
  }

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.intersection(this, typeHierarchy);
    }
    if (other is SetType) {
      List<ConcreteType> list = _intersectLists(types, other.types);
      final size = list.length;
      if (size == 0) {
        return const EmptyType();
      } else if (size == 1) {
        return list.single;
      } else {
        return new SetType(list);
      }
    } else if (other is ConcreteType) {
      for (var type in types) {
        if (type == other) return other;
        if (type.classId == other.classId) {
          return type.intersection(other, typeHierarchy);
        }
      }
      return EmptyType();
    } else if (other is ConeType) {
      return typeHierarchy
          .specializeTypeCone(other.dartType)
          .intersection(this, typeHierarchy);
    } else {
      throw 'Unexpected type $other';
    }
  }
}

/// Type representing a subtype cone. It contains instances of all
/// Dart types which extend, mix-in or implement [dartType].
/// TODO(alexmarkov): Introduce cones of types which extend but not implement.
class ConeType extends Type {
  final DartType dartType;

  ConeType(this.dartType) {
    assertx(dartType != null);
  }

  @override
  Class getConcreteClass(TypeHierarchy typeHierarchy) => typeHierarchy
      .specializeTypeCone(dartType)
      .getConcreteClass(typeHierarchy);

  @override
  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) =>
      typeHierarchy.isSubtype(this.dartType, dartType);

  bool isDefinitelySubtypeOfRuntimeType(
      TypeHierarchy typeHierarchy, RuntimeType other) {
    if (!typeHierarchy.isSubtype(dartType, other._type)) return false;
    if (dartType is InterfaceType) {
      return (dartType as InterfaceType).classNode.typeParameters.length == 0;
    }
    return true;
  }

  @override
  int get hashCode => (dartType.hashCode + 37) & kHashMask;

  @override
  bool operator ==(other) =>
      (other is ConeType) && (this.dartType == other.dartType);

  @override
  String toString() => "_T (${dartType})+";

  @override
  int get order => TypeOrder.Cone.index;

  @override
  bool get isSpecialized => false;

  @override
  Type specialize(TypeHierarchy typeHierarchy) =>
      typeHierarchy.specializeTypeCone(dartType);

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.union(this, typeHierarchy);
    }
    if (other is ConeType) {
      if (this == other) {
        return this;
      }
      if (typeHierarchy.isSubtype(other.dartType, this.dartType)) {
        return this;
      }
      if (typeHierarchy.isSubtype(this.dartType, other.dartType)) {
        return other;
      }
    } else if (other is ConcreteType) {
      if (typeHierarchy.isSubtype(
          InterfaceType(other.classNode), this.dartType)) {
        return this;
      }
    }
    return typeHierarchy
        .specializeTypeCone(dartType)
        .union(other, typeHierarchy);
  }

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.intersection(this, typeHierarchy);
    }
    if (other is ConeType) {
      if (this == other) {
        return this;
      }
      if (typeHierarchy.isSubtype(other.dartType, this.dartType)) {
        return other;
      }
      if (typeHierarchy.isSubtype(this.dartType, other.dartType)) {
        return this;
      }
    } else if (other is ConcreteType) {
      if (typeHierarchy.isSubtype(
          InterfaceType(other.classNode), this.dartType)) {
        return other;
      } else {
        return const EmptyType();
      }
    }
    return typeHierarchy
        .specializeTypeCone(dartType)
        .intersection(other, typeHierarchy);
  }
}

/// Abstract unique identifier of a Dart class.
/// Identifiers are comparable and used to provide ordering on classes.
abstract class ClassId<E extends ClassId<E>> implements Comparable<E> {
  const ClassId();
}

/// Simple implementation of [ClassId] based on int.
class IntClassId extends ClassId<IntClassId> {
  final int id;

  const IntClassId(this.id);

  @override
  int compareTo(IntClassId other) => id.compareTo(other.id);
}

/// Type representing a set of instances of a specific Dart class (no subtypes
/// or `null` object).
class ConcreteType extends Type implements Comparable<ConcreteType> {
  final ClassId classId;
  final Class classNode;
  int _hashCode;

  // May be null if there are no type arguments constraints. The type arguments
  // should represent type sets, i.e. `AnyType` or `RuntimeType`. The type
  // arguments vector is factored against the generic interfaces implemented by
  // the class (see [TypeHierarchy.factoredGenericInterfacesOf]).
  //
  // The 'typeArgs' vector is null for non-generic classes, even if they
  // implement a generic interface.
  //
  // 'numImmediateTypeArgs' is the length of the prefix of 'typeArgs' which
  // holds the type arguments to the class itself.
  final int numImmediateTypeArgs;
  final List<Type> ct_typeArgs;

  ConcreteType(this.classId, this.classNode, [List<Type> typeArgs_])
      : ct_typeArgs = typeArgs_,
        numImmediateTypeArgs =
            typeArgs_ != null ? classNode.typeParameters.length : 0 {
    // TODO(alexmarkov): support closures
    assertx(!classNode.isAbstract);
    assertx(ct_typeArgs == null || classNode.typeParameters.isNotEmpty);
    assertx(ct_typeArgs == null || ct_typeArgs.any((t) => t is RuntimeType));
  }

  ConcreteType get raw => new ConcreteType(classId, classNode, null);

  @override
  Class getConcreteClass(TypeHierarchy typeHierarchy) => classNode;

  @override
  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) {
    if (ct_typeArgs == null) {
      return typeHierarchy.isSubtype(InterfaceType(classNode), dartType);
    } else {
      // TODO(sjindel/tfa): Take type arguments into account. Currently we only
      // use this method for checking if a type is a subtype of int.
      assertx(dartType.toString() == "dart.core::int", details: "$dartType");
      return false;
    }
  }

  bool isDefinitelySubtypeOfRuntimeType(
      TypeHierarchy typeHierarchy, RuntimeType runtimeType) {
    if (!typeHierarchy.isSubtype(
        InterfaceType(this.classNode), runtimeType._type)) {
      return false;
    }
    if (runtimeType._type is! InterfaceType ||
        (runtimeType._type as InterfaceType).typeArguments.length == 0) {
      return true;
    }
    if (ct_typeArgs == null) return false;

    final interfaceOffset = typeHierarchy.genericInterfaceOffsetFor(
        classNode, (runtimeType._type as InterfaceType).classNode);

    assertx(ct_typeArgs.length - interfaceOffset >=
        runtimeType.numImmediateTypeArgs);

    for (int i = 0; i < runtimeType.numImmediateTypeArgs; ++i) {
      if (ct_typeArgs[i + interfaceOffset] == const AnyType()) return false;
      assertx(ct_typeArgs[i + interfaceOffset] is RuntimeType);
      if (!ct_typeArgs[i + interfaceOffset].isDefinitelySubtypeOfRuntimeType(
          typeHierarchy, runtimeType.rt_typeArgs[i])) {
        return false;
      }
    }
    return true;
  }

  @override
  int get hashCode => _hashCode ??= _computeHashCode();

  int _computeHashCode() {
    int hash = classId.hashCode ^ 0x1234 & kHashMask;
    // We only need to hash the first type arguments vector, since the type
    // arguments of the implemented interfaces are implied by it.
    for (int i = 0; i < numImmediateTypeArgs; ++i) {
      hash = (((hash * 31) & kHashMask) + ct_typeArgs[i].hashCode) & kHashMask;
    }
    return hash;
  }

  @override
  bool operator ==(other) {
    if (other is ConcreteType) {
      if (this.classId != other.classId ||
          this.numImmediateTypeArgs != other.numImmediateTypeArgs) {
        return false;
      }
      if (this.ct_typeArgs != null) {
        for (int i = 0; i < numImmediateTypeArgs; ++i) {
          if (this.ct_typeArgs[i] != other.ct_typeArgs[i]) {
            return false;
          }
        }
      }
      return true;
    } else {
      return false;
    }
  }

  // Note that this may return 0 for concrete types which are not equal if the
  // difference is only in type arguments.
  @override
  int compareTo(ConcreteType other) => classId.compareTo(other.classId);

  @override
  String toString() => ct_typeArgs == null
      ? "_T (${classNode})"
      : "_T (${classNode}<${ct_typeArgs.take(numImmediateTypeArgs).join(', ')}>)";

  @override
  int get order => TypeOrder.Concrete.index;

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.union(this, typeHierarchy);
    }
    if (other is ConcreteType) {
      if (this == other) {
        return this;
      } else if (this.classId != other.classId) {
        final types = (this.classId.compareTo(other.classId) < 0)
            ? <ConcreteType>[this, other]
            : <ConcreteType>[other, this];
        return new SetType(types);
      } else {
        assertx(ct_typeArgs != null || other.ct_typeArgs != null);
        return raw;
      }
    } else {
      throw 'Unexpected type $other';
    }
  }

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other.order < this.order) {
      return other.intersection(this, typeHierarchy);
    }
    if (other is ConcreteType) {
      if (this == other) {
        return this;
      }
      if (this.classId != other.classId) {
        return EmptyType();
      }
      assertx(ct_typeArgs != null || other.ct_typeArgs != null);
      if (ct_typeArgs == null) {
        return other;
      } else if (other.ct_typeArgs == null) {
        return this;
      }

      final mergedTypeArgs = new List<Type>(ct_typeArgs.length);
      bool hasRuntimeType = false;
      for (int i = 0; i < ct_typeArgs.length; ++i) {
        final merged =
            ct_typeArgs[i].intersection(other.ct_typeArgs[i], typeHierarchy);
        if (merged is EmptyType) {
          return EmptyType();
        } else if (merged is RuntimeType) {
          hasRuntimeType = true;
        }
        mergedTypeArgs[i] = merged;
      }
      if (!hasRuntimeType) return raw;
      return new ConcreteType(classId, classNode, mergedTypeArgs);
    } else {
      throw 'Unexpected type $other';
    }
  }
}

// Unlike the other 'Type's, this represents a single type, not a set of
// values. It is used as the right-hand-side of type-tests.
//
// The type arguments are represented in a form that is factored against the
// generic interfaces implemented by the type to enable efficient type-test
// against its interfaces. See 'TypeHierarchy.factoredGenericInterfacesOf' for
// more details.
//
// This factored representation can have cycles for some types:
//
//   class num implements Comparable<num> {}
//   class A<T> extends Comparable<A<T>> {}
//
// To avoid these cycles, we approximate generic super-bounded types (the second
// case), so the representation for 'A<String>' would be simply 'AnyType'.
// However, approximating non-generic types like 'int' and 'num' (the first
// case) would be too coarse, so we leave an null 'typeArgs' field for these
// types. As a result, when doing an 'isDefinitelySubtypeOfRuntimeType' against
// their interfaces (e.g. 'int' vs 'Comparable<int>') we approximate the result
// as 'false'.
//
// So, the invariant about 'typeArgs' is that they will be 'null' iff the class
// is non-generic, and non-null (with at least one vector) otherwise.
class RuntimeType extends Type {
  final DartType _type; // Doesn't contain type args.
  final int numImmediateTypeArgs;
  final List<RuntimeType> rt_typeArgs;

  RuntimeType(DartType type, this.rt_typeArgs)
      : _type = type,
        numImmediateTypeArgs =
            type is InterfaceType ? type.classNode.typeParameters.length : 0 {
    assertx(_type is DynamicType || _type is InterfaceType);
    if (_type is InterfaceType &&
        (_type as InterfaceType).typeArguments.length > 0) {
      assertx(rt_typeArgs != null);
      assertx(rt_typeArgs.length > 0);
      assertx((_type as InterfaceType)
          .typeArguments
          .every((t) => t == const DynamicType()));
    } else {
      assertx(rt_typeArgs == null);
    }
  }

  int get order => TypeOrder.RuntimeType.index;

  DartType get representedType {
    if (_type is InterfaceType && rt_typeArgs != null) {
      final klass = (_type as InterfaceType).classNode;
      final typeArguments = rt_typeArgs
          .take(klass.typeParameters.length)
          .map((pt) => pt.representedType)
          .toList();
      return new InterfaceType(klass, typeArguments);
    } else {
      return _type;
    }
  }

  @override
  int get hashCode {
    int hash = _type.hashCode ^ 0x1234 & kHashMask;
    // Only hash by the type arguments of the class. The type arguments of
    // supertypes are are implied by them.
    for (int i = 0; i < numImmediateTypeArgs; ++i) {
      hash = (((hash * 31) & kHashMask) + rt_typeArgs[i].hashCode) & kHashMask;
    }
    return hash;
  }

  @override
  operator ==(other) {
    if (other is RuntimeType) {
      if (other._type != _type) return false;
      for (int i = 0; i < numImmediateTypeArgs; ++i) {
        if (rt_typeArgs[i] != other.rt_typeArgs[i]) return false;
      }
      return true;
    }
    return false;
  }

  @override
  String toString() {
    final head = "$_type";
    if (numImmediateTypeArgs == 0) return head;
    final typeArgsStrs =
        rt_typeArgs.take(numImmediateTypeArgs).map((t) => "$t").join(", ");
    return "_TS {$head<$typeArgsStrs>}";
  }

  @override
  bool get isSpecialized =>
      throw "ERROR: RuntimeType does not support isSpecialized.";

  @override
  bool isSubtypeOf(TypeHierarchy typeHierarchy, DartType dartType) =>
      throw "ERROR: RuntimeType does not support isSubtypeOf.";

  @override
  Type union(Type other, TypeHierarchy typeHierarchy) =>
      throw "ERROR: RuntimeType does not support union.";

  @override
  Type intersection(Type other, TypeHierarchy typeHierarchy) {
    if (other is AnyType) {
      return this;
    } else if (other is RuntimeType) {
      return this == other ? this : const EmptyType();
    }
    throw "ERROR: RuntimeType cannot intersect with ${other.runtimeType}";
  }

  @override
  Type specialize(TypeHierarchy typeHierarchy) =>
      throw "ERROR: RuntimeType does not support specialize.";

  @override
  Class getConcreteClass(TypeHierarchy typeHierarchy) =>
      throw "ERROR: ConcreteClass does not support getConcreteClass.";

  bool isDefinitelySubtypeOfRuntimeType(
      TypeHierarchy typeHierarchy, RuntimeType runtimeType) {
    if (!typeHierarchy.isSubtype(this._type, runtimeType._type)) return false;
    if (_type is! InterfaceType || runtimeType._type is! InterfaceType) {
      return true;
    }

    final thisClass = (_type as InterfaceType).classNode;
    final otherClass = (runtimeType._type as InterfaceType).classNode;

    if (numImmediateTypeArgs > 0) {
      if (rt_typeArgs == null) return false;
      final interfaceOffset =
          typeHierarchy.genericInterfaceOffsetFor(thisClass, otherClass);
      assertx(rt_typeArgs.length - interfaceOffset >=
          runtimeType.numImmediateTypeArgs);
      for (int i = 0; i < runtimeType.numImmediateTypeArgs; ++i) {
        if (!rt_typeArgs[interfaceOffset + i].isDefinitelySubtypeOfRuntimeType(
            typeHierarchy, runtimeType.rt_typeArgs[i])) {
          return false;
        }
      }
    }
    return true;
  }
}
