# Contributing to Joe-Mon

### submitting changes

Changes should be submitted through the 'pull request' feature of github. Reasonable pull requests for bug fixes and features will be looked at and considered, and, if accepted, ultimately merged into Joe-Mon.

### reporting a bug or requesting features

Bug reports and feature requests should use the 'issues' section of github. 

For a bug report, try to describe the bug as best as possible. If possible, include a surefire method to reproduce the issue.

For a feature request, developers will work out the details of the proposed addition through the issue.

### writing new features

Generally speaking, multiple people working on the same assembly language program presents issues. However, due to the nature of Joe-Mon as a command oriented environment, individual commands can be developed more or less independently. Therefore, new features will most often be related to the addition of a command to Joe-Mon. New commands must be added to the command table (see label 'cmd_table:' in the source)

There are several functions which are useful as a minimal console API and are documented with comments in the source: putc, puts, getc, gets.

Additionally, there are useful constants scattered around the source which are (unfortunately) mostly undocumented. Ex, bounds for CMP2 instruction for lower case ASCII characters at label 'lowercasebound'

Lastly, there is a set of global variables used by the monitor listed in an offset section at the end of the program source. These are partially documented in the comments.

Documentation is not yet available for much of the details of adding functionality to Joe-Mon, but looking at these locations in the source will give an idea until proper documentation is available.