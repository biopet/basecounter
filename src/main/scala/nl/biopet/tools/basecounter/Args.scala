package nl.biopet.tools.basecounter

import java.io.File

case class Args(refFlat: File = null,
                outputDir: File = null,
                bamFile: File = null,
                prefix: String = "output")
