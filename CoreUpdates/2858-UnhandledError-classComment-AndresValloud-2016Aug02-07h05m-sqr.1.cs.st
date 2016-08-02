'From Cuis 4.2 of 25 July 2013 [latest update: #2850] on 2 August 2016 at 7:05:44 am'!

!UnhandledError commentStamp: '<historical>' prior: 0!
This exception represents an error exception that has gone unhandled.  Unhandled error conditions are fundamentally different from error exceptions, because error exceptions may have default handlers that address the error condition (by e.g. retrying the operation).  The job of unhandled errors is to report the original problem.  This reporting can be done in a variety of ways.  For example, in everyday practice, unhandled errors open the debugger.

Note the common practice of "catching all errors" with code such as this:

	[some code]
		on: Error
		do: [:ex | ex return]
		
is doubly problematic.  First, there is no specificity to the expected exceptions arising from the protected block.  Second, the handler block will prevent the exception's default handler from running, which may resolve or otherwise react to the error condition.  If one really wants to catch unhandled errors, the code should read like this instead:

	[some code]
		on: UnhandledError
		do: [:ex | ex return]!

