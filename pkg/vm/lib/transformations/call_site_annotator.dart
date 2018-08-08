// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library vm.transformations.call_site_annotator;

import 'package:kernel/ast.dart';
import 'package:kernel/class_hierarchy.dart' show ClassHierarchy;
import 'package:kernel/core_types.dart' show CoreTypes;
import 'package:kernel/type_environment.dart' show TypeEnvironment;

import '../metadata/call_site_attributes.dart';

/// Assumes strong mode and closed world. If a procedure can not be riched
/// via dynamic invocation from anywhere then annotates it with appropriate
/// [ProcedureAttributeMetadata] annotation.
Component transformComponent(Component component) {
  void ignoreAmbiguousSupertypes(Class cls, Supertype a, Supertype b) {}
  ClassHierarchy hierarchy = new ClassHierarchy(component,
      onAmbiguousSupertypes: ignoreAmbiguousSupertypes);
  new AnnotateWithStaticTypes(component, new CoreTypes(component), hierarchy).visitComponent(component);
  return component;
}

class AnnotateWithStaticTypes extends RecursiveVisitor<Null> {
  final CallSiteAttributesMetadataRepository _metadata;
  final TypeEnvironment env;

  AnnotateWithStaticTypes(Component component, CoreTypes coreTypes, ClassHierarchy hierarchy)
      : _metadata = new CallSiteAttributesMetadataRepository(),
        env = new TypeEnvironment(coreTypes, hierarchy, strongMode: true) {
    component.addMetadataRepository(_metadata);
  }

  static bool containsGenericCovariantImpl(List<VariableDeclaration> decls) {
    return decls.any((p) => p.isGenericCovariantImpl);
  }

  static bool hasGenericCovariantParameters(Member member) {
    if (member is Procedure) {
      return containsGenericCovariantImpl(member.function.positionalParameters) || containsGenericCovariantImpl(member.function.namedParameters);
    }

    return false;
  }

  @override
  visitProcedure(Procedure proc) {
    if (!proc.isStatic) {
      env.thisType = proc.enclosingClass?.thisType;
    }
    super.visitProcedure(proc);
    env.thisType = null;
  }

  @override
  visitConstructor(Constructor proc) {
    env.thisType = proc.enclosingClass?.thisType;
    super.visitConstructor(proc);
    env.thisType = null;
  }

  @override
  visitMethodInvocation(MethodInvocation node) {
    super.visitMethodInvocation(node);

    if (node.interfaceTarget != null &&
        hasGenericCovariantParameters(node.interfaceTarget)) {
      _metadata.mapping[node] = new CallSiteAttributesMetadata(receiverType: node.receiver.getStaticType(env));
    }
  }
}
