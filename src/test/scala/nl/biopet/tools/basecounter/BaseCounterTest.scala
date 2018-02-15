/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.basecounter

import java.io.File
import java.nio.file.Paths

import com.google.common.io.Files
import htsjdk.samtools.{
  SAMFileHeader,
  SAMLineParser,
  SAMReadGroupRecord,
  SAMSequenceRecord
}
import nl.biopet.utils.test.tools.ToolTest
import nl.biopet.tools.basecounter.BaseCounter.{Counts, GeneCount}
import org.testng.annotations.Test
import picard.annotation.Gene

import scala.collection.JavaConversions._

class BaseCounterTest extends ToolTest[Args] {
  def toolCommand: BaseCounter.type = BaseCounter

  import BaseCounterTest._

  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      BaseCounter.main(Array())
    }
  }

  @Test
  def testCountsClass(): Unit = {
    val counts = new Counts
    counts.antiSenseBases shouldBe 0
    counts.antiSenseReads shouldBe 0
    counts.senseBases shouldBe 0
    counts.senseReads shouldBe 0
    counts.totalBases shouldBe 0
    counts.totalReads shouldBe 0

    counts.antiSenseBases = 1
    counts.senseBases = 2
    counts.totalBases shouldBe 3

    counts.antiSenseReads = 1
    counts.senseReads = 2
    counts.totalReads shouldBe 3
  }

  @Test
  def testBamRecordBasesOverlapBlocks(): Unit = {
    val read = BaseCounterTest.lineParser.parseLine(
      "r02\t0\tchrQ\t50\t60\t4M2D4M\t*\t0\t0\tTACGTGTA\tEEFFGGII\tRG:Z:001")
    BaseCounter.bamRecordBasesOverlap(read, 40, 70) shouldBe 8
    BaseCounter.bamRecordBasesOverlap(read, 50, 59) shouldBe 8
    BaseCounter.bamRecordBasesOverlap(read, 50, 55) shouldBe 4
    BaseCounter.bamRecordBasesOverlap(read, 55, 60) shouldBe 4
    BaseCounter.bamRecordBasesOverlap(read, 10, 20) shouldBe 0
    BaseCounter.bamRecordBasesOverlap(read, 40, 49) shouldBe 0
    BaseCounter.bamRecordBasesOverlap(read, 40, 50) shouldBe 1
    BaseCounter.bamRecordBasesOverlap(read, 40, 51) shouldBe 2
    BaseCounter.bamRecordBasesOverlap(read, 58, 70) shouldBe 2
    BaseCounter.bamRecordBasesOverlap(read, 59, 70) shouldBe 1
    BaseCounter.bamRecordBasesOverlap(read, 60, 70) shouldBe 0
  }

  @Test
  def testBamRecordBasesOverlap(): Unit = {
    val read = BaseCounterTest.lineParser.parseLine(
      "r02\t0\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    BaseCounter.bamRecordBasesOverlap(read, 40, 70) shouldBe 10
    BaseCounter.bamRecordBasesOverlap(read, 50, 59) shouldBe 10
    BaseCounter.bamRecordBasesOverlap(read, 50, 55) shouldBe 6
    BaseCounter.bamRecordBasesOverlap(read, 55, 60) shouldBe 5
    BaseCounter.bamRecordBasesOverlap(read, 10, 20) shouldBe 0
    BaseCounter.bamRecordBasesOverlap(read, 40, 49) shouldBe 0
    BaseCounter.bamRecordBasesOverlap(read, 40, 50) shouldBe 1
    BaseCounter.bamRecordBasesOverlap(read, 40, 51) shouldBe 2
    BaseCounter.bamRecordBasesOverlap(read, 58, 70) shouldBe 2
    BaseCounter.bamRecordBasesOverlap(read, 59, 70) shouldBe 1
    BaseCounter.bamRecordBasesOverlap(read, 60, 70) shouldBe 0

    val counts = new Counts
    BaseCounter.bamRecordBasesOverlap(read, 40, 70, counts, sense = true)
    counts.senseBases shouldBe 10
    counts.antiSenseBases shouldBe 0
    counts.senseReads shouldBe 1
    counts.antiSenseReads shouldBe 0

    BaseCounter.bamRecordBasesOverlap(read, 50, 54, counts, sense = false)
    counts.senseBases shouldBe 10
    counts.antiSenseBases shouldBe 5
    counts.senseReads shouldBe 1
    counts.antiSenseReads shouldBe 1
  }

  @Test
  def testSamRecordStrand(): Unit = {
    val readPlusUnpaired = BaseCounterTest.lineParser.parseLine(
      "r02\t0\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readMinUnpaired = BaseCounterTest.lineParser.parseLine(
      "r02\t16\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readPlusPairedR1 = BaseCounterTest.lineParser.parseLine(
      "r02\t73\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readMinPairedR1 = BaseCounterTest.lineParser.parseLine(
      "r02\t89\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readPlusPairedR2 = BaseCounterTest.lineParser.parseLine(
      "r02\t137\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readMinPairedR2 = BaseCounterTest.lineParser.parseLine(
      "r02\t153\tchrQ\t50\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")

    BaseCounter.samRecordStrand(readPlusUnpaired, strand = true) shouldBe false
    BaseCounter.samRecordStrand(readMinUnpaired, strand = true) shouldBe true
    BaseCounter.samRecordStrand(readPlusPairedR1, strand = true) shouldBe false
    BaseCounter.samRecordStrand(readMinPairedR1, strand = true) shouldBe true
    BaseCounter.samRecordStrand(readPlusPairedR2, strand = true) shouldBe true
    BaseCounter.samRecordStrand(readMinPairedR2, strand = true) shouldBe false

    BaseCounter.samRecordStrand(readPlusUnpaired, strand = false) shouldBe true
    BaseCounter.samRecordStrand(readMinUnpaired, strand = false) shouldBe false
    BaseCounter.samRecordStrand(readPlusPairedR1, strand = false) shouldBe true
    BaseCounter.samRecordStrand(readMinPairedR1, strand = false) shouldBe false
    BaseCounter.samRecordStrand(readPlusPairedR2, strand = false) shouldBe false
    BaseCounter.samRecordStrand(readMinPairedR2, strand = false) shouldBe true

    BaseCounter.samRecordStrand(readPlusUnpaired, geneA) shouldBe false
    BaseCounter.samRecordStrand(readMinUnpaired, geneA) shouldBe true
    BaseCounter.samRecordStrand(readPlusPairedR1, geneA) shouldBe false
    BaseCounter.samRecordStrand(readMinPairedR1, geneA) shouldBe true
    BaseCounter.samRecordStrand(readPlusPairedR2, geneA) shouldBe true
    BaseCounter.samRecordStrand(readMinPairedR2, geneA) shouldBe false

    BaseCounter.samRecordStrand(readPlusUnpaired, geneC) shouldBe true
    BaseCounter.samRecordStrand(readMinUnpaired, geneC) shouldBe false
    BaseCounter.samRecordStrand(readPlusPairedR1, geneC) shouldBe true
    BaseCounter.samRecordStrand(readMinPairedR1, geneC) shouldBe false
    BaseCounter.samRecordStrand(readPlusPairedR2, geneC) shouldBe false
    BaseCounter.samRecordStrand(readMinPairedR2, geneC) shouldBe true
  }

  @Test
  def testGeneCount(): Unit = {
    val readPlus = BaseCounterTest.lineParser.parseLine(
      "r02\t0\tchrQ\t101\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val readMin = BaseCounterTest.lineParser.parseLine(
      "r02\t16\tchrQ\t101\t60\t10M\t*\t0\t0\tTACGTACGTA\tEEFFGGHHII\tRG:Z:001")
    val geneCount = new GeneCount(geneA)

    geneCount.gene shouldBe geneA
    geneCount.transcripts.size shouldBe 1
    geneCount.transcripts.head.exonCounts.length shouldBe 4
    geneCount.transcripts.head.intronCounts.size shouldBe 3

    geneCount.addRecord(readPlus, BaseCounter.samRecordStrand(readPlus, geneA))
    geneCount.exonCounts.map(_.counts.senseBases).sum shouldBe 0
    geneCount.exonCounts.map(_.counts.antiSenseBases).sum shouldBe 10
    geneCount.addRecord(readMin, BaseCounter.samRecordStrand(readMin, geneA))
    geneCount.exonCounts.map(_.counts.senseBases).sum shouldBe 10
    geneCount.exonCounts.map(_.counts.antiSenseBases).sum shouldBe 10
  }

  @Test
  def testGroupGenesOnOverlap(): Unit = {
    assert(
      BaseCounter
        .groupGenesOnOverlap(geneC :: geneD :: Nil)("chrQ")
        .contains(List(geneC)))
    assert(
      BaseCounter
        .groupGenesOnOverlap(geneC :: geneD :: Nil)("chrQ")
        .contains(List(geneD)))
    assert(
      !BaseCounter
        .groupGenesOnOverlap(geneC :: geneD :: Nil)("chrQ")
        .contains(List(geneD, geneC)))

    assert(
      !BaseCounter
        .groupGenesOnOverlap(geneC :: geneA :: Nil)("chrQ")
        .contains(List(geneA)))
    assert(
      !BaseCounter
        .groupGenesOnOverlap(geneC :: geneA :: Nil)("chrQ")
        .contains(List(geneC)))
    assert(
      BaseCounter
        .groupGenesOnOverlap(geneC :: geneA :: Nil)("chrQ")
        .contains(List(geneA, geneC)))
  }

  @Test
  def testCreateMetaExonCounts(): Unit = {
    val ab = BaseCounter.createMetaExonCounts(geneA :: geneB :: Nil)
    ab.size shouldBe 9
    assert(
      ab.exists(x => x._1 == "geneA" && x._2.start == 101 && x._2.end == 120))
    assert(
      ab.exists(x => x._1 == "geneA" && x._2.start == 131 && x._2.end == 140))

    assert(ab.exists(x =>
      x._1 == "geneA,geneB" && x._2.start == 151 && x._2.end == 160))
    assert(
      ab.exists(x => x._1 == "geneB" && x._2.start == 161 && x._2.end == 170))
    assert(
      ab.exists(x => x._1 == "geneA" && x._2.start == 171 && x._2.end == 180))
    assert(ab.exists(x =>
      x._1 == "geneA,geneB" && x._2.start == 181 && x._2.end == 190))
    assert(
      ab.exists(x => x._1 == "geneA" && x._2.start == 191 && x._2.end == 200))

    assert(
      ab.exists(x => x._1 == "geneB" && x._2.start == 201 && x._2.end == 210))
    assert(
      ab.exists(x => x._1 == "geneB" && x._2.start == 221 && x._2.end == 250))
  }

  @Test
  def testMain(): Unit = {
    val outputDir = Files.createTempDir()
    outputDir.deleteOnExit()
    val prefix = "test"
    val bamFile = new File(resourcePath("/empty.bam"))
    val refflat = new File(resourcePath("/chrQ.refflat"))
    BaseCounter.main(
      Array("-o",
            outputDir.getAbsolutePath,
            "-p",
            prefix,
            "-b",
            bamFile.getAbsolutePath,
            "-r",
            refflat.getAbsolutePath))
    outputDir.list().length shouldBe 35
  }
}

object BaseCounterTest {
  val lineParser: SAMLineParser = {
    val header = new SAMFileHeader
    header.addSequence(new SAMSequenceRecord("chrQ", 10000))
    header.addSequence(new SAMSequenceRecord("chrR", 10000))
    header.addReadGroup(new SAMReadGroupRecord("001"))

    new SAMLineParser(header)
  }

  val geneA: Gene = {
    val gene = new Gene("chrQ", 101, 200, false, "geneA")
    gene.addTranscript("A1", 101, 200, 111, 190, 4)
    for (transcript <- gene) {
      transcript.name match {
        case "A1" =>
          transcript.addExon(101, 120)
          transcript.addExon(131, 140)
          transcript.addExon(151, 160)
          transcript.addExon(171, 200)
      }
    }
    gene
  }

  val geneB: Gene = {
    val gene = new Gene("chrQ", 151, 250, false, "geneB")
    gene.addTranscript("A1", 151, 250, 161, 240, 4)
    for (transcript <- gene) {
      transcript.name match {
        case "A1" =>
          transcript.addExon(151, 170)
          transcript.addExon(181, 190)
          transcript.addExon(201, 210)
          transcript.addExon(221, 250)
      }
    }
    gene
  }

  val geneC = new Gene("chrQ", 101, 300, true, "geneC")
  val geneD = new Gene("chrQ", 301, 500, true, "geneD")

}
