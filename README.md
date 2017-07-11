# Sisyphos - Multiple Choice Entering Tool
A simple command-line based alternative to enter given answers to
multiple choice questions. Sisyphos conforms to the format of the
Aarhus [Multiple Choice Tool](http://www.brics.dk/Multiple/) and
thus can be used as an alternative to their GUI based tool.

To use it, first download the jar file under [releases](https://github.com/b-studios/sisyphos/releases).

Then run:

```
java -jar sisyphos.jar my-file.scores
```

Sisyphos will then ask you a few questions about the shape of the
exam (for some simple validations) and your set to go!

## Available commands
Once you are in answer entering mode there is a set of commands. The
following is the output of running `:help`:

```
Some commands can be abbreviated; Abbreviation given in parenthesis.
:edit student-id|exam-id     edit the meta data of the current exam
:exit                        exit the exam-entering, do not save current exam
:help (:h)                   shows this summary of available commands
:reenter [n] (:r)            reenter the answers to the n-last questions (n=1, if omitted)
```

## Text-to-Speech
To reduce the error rate of entering answers even more, we added a simple TTS that reads out the entered options. For now this is only supported on Mac OSX. To use the variant, for now, you have to checkout the [tts branch](https://github.com/b-studios/sisyphos/tree/tts) and run `sbt oneJar` to compile build the project. 

