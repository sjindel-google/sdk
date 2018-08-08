// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library vm.metadata.call_site_attributes;

import 'package:kernel/ast.dart';

/// Metadata for annotating procedures with various attributes.
class CallSiteAttributesMetadata {
  final DartType receiverType;

  const CallSiteAttributesMetadata(
      {this.receiverType});

  @override
  String toString() => "receiverType:$receiverType";
}

/// Repository for [CallSiteAttributesMetadata].
class CallSiteAttributesMetadataRepository
    extends MetadataRepository<CallSiteAttributesMetadata> {
  @override
  final String tag = 'vm.call-site-attributes.metadata';

  @override
  final Map<TreeNode, CallSiteAttributesMetadata> mapping =
      <TreeNode, CallSiteAttributesMetadata>{};

  @override
  void writeToBinary(
      CallSiteAttributesMetadata metadata, Node node, BinarySink sink) {
    sink.writeDartType(metadata.receiverType);
  }

  @override
  CallSiteAttributesMetadata readFromBinary(Node node, BinarySource source) {
    final type = source.readDartType();
    return new CallSiteAttributesMetadata(receiverType: type);
  }
}
