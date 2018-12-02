// build.sc
import mill._, scalalib._, publish._

object perceptron extends ScalaModule with PublishModule {
  def scalaVersion = "2.12.4"
  def publishVersion = "0.0.1"

  def ivyDeps = Agg(
    ivy"org.scalanlp:breeze_2.12:1.0-RC2",
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}"
  )

  def pomSettings = PomSettings(
    description = "Hello",
    organization = "org.kaygun",
    url = "https://github.com/kaygun/perceptron",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("kaygun", "perceptron"),
    developers = Seq(
      Developer("kaygun", "Atabey Kaygun","https://github.com/kaygun")
    )
  )
}
