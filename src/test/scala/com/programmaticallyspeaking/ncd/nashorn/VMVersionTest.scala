package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class VMVersionTest extends UnitTest with TableDrivenPropertyChecks {

  "VMVersion" - {
    "parse" - {

      val cases = Table(
        ("desc", "input", "expected"),
        ("Java 8", "1.8.0_151", VMVersion(1, Some(8), Some(0), Some(151), false)),
        ("Java 9", "9.0.1", VMVersion(9, Some(0), Some(1), None, false)),
        ("Java 10", "10", VMVersion(10, None, None, None, false)),
        ("Java 12 EA", "12-ea", VMVersion(12, None, None, None, true))
      )

      forAll(cases) { (desc, input, expected) =>
        s"parses version string '$input' ($desc)" in {
          val actual = VMVersion.parse(input)
          actual should be(expected)
        }
      }
    }

    "knownBugs" - {
      val B_8179072_cases = Table(
        ("version", "has_the_bug"),
        ("1.8.0_131", true),
        ("1.8.0_132", false),
        ("9.0.1", false)
      )

      def testForBug(version: String, bug: KnownBug.EnumVal, expected: Boolean) = {
        val ver = VMVersion.parse(version)
        if (expected)
          ver.knownBugs should contain (bug)
        else
          ver.knownBugs should not contain (bug)
      }

      forAll(B_8179072_cases) { (version, has_the_bug) =>
        val verb = if (has_the_bug) "includes" else "excludes"
        s"$verb JDK_8179072 for version $version" in {
          testForBug(version, KnownBug.JDK_8179072_Abstract_Method_Error, has_the_bug)
        }
      }

      val B_8187143_cases = Table(
        ("version", "has_the_bug"),
        ("1.8.0_144", false),
        ("1.8.1_1", true),
        ("1.8.0_151", true),
        ("9.0.1", true),
        ("10", true)
      )

      forAll(B_8187143_cases) { (version, has_the_bug) =>
        val verb = if (has_the_bug) "includes" else "excludes"
        s"$verb JDK_8187143 for version $version" in {
          testForBug(version, KnownBug.JDK_8187143_Frame_Restart_Crash, has_the_bug)
        }
      }

    }
  }
}
