FROM clojure:temurin-21-lein-alpine AS builder
WORKDIR /usr/src/app
COPY project.clj /usr/src/app/
RUN lein deps
COPY . .
RUN lein uberjar

FROM eclipse-temurin:21-jre-alpine
WORKDIR /usr/src/app
COPY --from=builder /usr/src/app/target/bowtie-m3-standalone.jar .
CMD ["java", "-Dpolyglot.engine.WarnInterpreterOnly=false", "-Dorg.slf4j.simpleLogger.defaultLogLevel=off", "-jar", "bowtie-m3-standalone.jar"]
