// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:kernel/kernel.dart'
    show LibraryDependency, Member, MetadataRepository, NamedNode;

/// The collector to add target specific metadata to.
abstract class MetadataCollector {
  /// Metadata is remembered in this repository, so that when it is added
  /// to a component, metadata is serialized with the component.
  MetadataRepository get repository;

  void setConstructorNameOffset(Member node, Object name);

  void setDocumentationComment(NamedNode node, String comment);

  void setImportPrefixOffset(LibraryDependency node, int offset);
}
