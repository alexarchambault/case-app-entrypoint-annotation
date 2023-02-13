import $ivy.`io.chris-kipp::mill-ci-release::0.1.5`

import io.kipp.mill.ci.release.CiReleaseModule
import mill.scalajslib._
import mill.scalalib._
import mill.scalalib.publish._

object Deps {
  def scala = "3.3.0-RC2"
  def scalaJs = "1.13.0"
}

object entrypoint extends Module {
  object jvm extends Entrypoint {
    def sources = T.sources {
      super.sources() ++ Seq(
        PathRef(millSourcePath / os.up / os.up / "src"),
        PathRef(millSourcePath / os.up / os.up / "jvm" / "src")
      )
    }
  }
  object js extends Entrypoint with ScalaJSModule {
    def scalaJSVersion = Deps.scalaJs
    def sources = T.sources {
      super.sources() ++ Seq(
        PathRef(millSourcePath / os.up / os.up / "src"),
        PathRef(millSourcePath / os.up / os.up / "js" / "src")
      )
    }
  }

  trait Entrypoint extends ScalaModule with CiReleaseModule {
    def scalaVersion = Deps.scala
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.github.alexarchambault::case-app::2.1.0-M23"
      // ivy"com.lihaoyi::pprint::0.8.1"
    )
    def compileIvyDeps = super.compileIvyDeps() ++ Agg(
      ivy"org.scala-lang::scala3-compiler:${Deps.scala}"
    )

    def artifactName = "case-app-entrypoint-annotation"

    def pomSettings = PomSettings(
      description = artifactName(),
      organization = "com.github.alexarchambault",
      url = "https://github.com/alexarchambault/case-app-entrypoint-annotation",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("alexarchambault", "case-app-entrypoint-annotation"),
      developers = Seq(
        Developer("alexarchambault", "Alex Archambault","https://github.com/alexarchambault")
      )
    )
  }
}
