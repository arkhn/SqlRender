checksum <- rJava::J("org.ohdsi.sql.JarChecksum", "computeJarChecksum")
write(checksum, file.path("inst", "csv", "jarChecksum.txt"))
