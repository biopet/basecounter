package nl.biopet.tools.basecounter

import java.io.File

import nl.biopet.utils.tool.AbstractOptParser

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[File]('r', "refFlat") required () valueName "<file>" action { (x, c) =>
    c.copy(refFlat = x)
  } text "refFlat file. Mandatory"
  opt[File]('o', "outputDir") required () valueName "<directory>" action {
    (x, c) =>
      c.copy(outputDir = x)
  } text "Output directory. Mandatory"
  opt[File]('b', "bam") required () valueName "<file>" action { (x, c) =>
    c.copy(bamFile = x)
  } text "Bam file. Mandatory"
  opt[String]('p', "prefix") valueName "<prefix>" action { (x, c) =>
    c.copy(prefix = x)
  } text "The prefix for the output files"
}
