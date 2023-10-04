FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.9.6_3.3.1

WORKDIR /root
EXPOSE 8080
COPY . .
CMD ["sbt", "run"]
