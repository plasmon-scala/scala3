//> using scala "3.7.0"
//> using jvm "17"
//> using dep "org.lz4:lz4-java:1.8.0"
//> using dep "io.get-coursier:interface:1.0.18"
//> using dep "io.github.plasmon-scala:mtags-interfaces:1.6.1,exclude=org.eclipse.lsp4j%org.eclipse.lsp4j,exclude=org.eclipse.lsp4j%org.eclipse.lsp4j.jsonrpc"
//> using dep "io.github.plasmon-scala:mtags_2.13.16:1.6.1"
//> using dep "org.eclipse.lsp4j:org.eclipse.lsp4j:0.23.1"
//> using dep "org.scala-lang:scala3-compiler_3:3.7.0"
//> using options "-source" "3.3" "-Yexplicit-nulls" "-Wsafe-init"
//> using publish.organization "io.github.plasmon-scala"
//> using publish.name "scala3-presentation-compiler"

//> using publish.ci.computeVersion "git:tag:../../../"
//> using publish.computeVersion "git:tag:../../../"
//> using publish.ci.repository "central"
//> using publish.license "Apache-2.0"
//> using publish.url "https://github.com/plasmon-scala/scala3"
//> using publish.versionControl "github:plasmon-scala/scala3"
//> using publish.developer "Alex Archambault||https://github.com/alexarchambault"

//> using publish.ci.user env:PUBLISH_USER
//> using publish.ci.password env:PUBLISH_PASSWORD
//> using publish.ci.publicKey env:PUBLISH_PUBLIC_KEY
//> using publish.ci.secretKey env:PUBLISH_SECRET_KEY
//> using publish.ci.secretKeyPassword env:PUBLISH_SECRET_KEY_PASSWORD
