(defproject criterium "0.4.6-SNAPSHOT"
  :description "Benchmarking library"
  :url "https://github.com/hugoduncan/criterium"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:hugoduncan/criterium.git"}
  :dependencies [[org.openjdk.jmh/jmh-core "1.23"]]
  :local-repo-classpath true
  :java-source-paths ["src"])
