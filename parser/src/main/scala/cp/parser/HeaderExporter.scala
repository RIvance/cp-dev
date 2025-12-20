package cp.parser

import cp.core.{Module, ModuleHeader, Type}

class HeaderExporter(module: Module) {
  def emitHeader: String = {
    val headerBuilder = new StringBuilder

    // Imports
    module.dependencies.toSeq.sortBy(_.namespace.toString).foreach { dep =>
      headerBuilder.append(s"import ${dep.namespace.path.mkString("::")};\n")
    }

    // Types (e.g., type A = xxx)
    module.types.toSeq.sortBy(_._1).foreach { case (name, ty) =>
      headerBuilder.append(s"type $name = ${ty.normalize.toString};\n")
    }

    // Externs (e.g., extern a: A)
    module.symbols.toSeq.sortBy(_._1).foreach { case (name, ty) =>
      headerBuilder.append(s"extern $name: ${ty.normalize.toString};\n")
    }

    headerBuilder.toString()
  }
}
