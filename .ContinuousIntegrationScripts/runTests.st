|suite result exitCode|

Utilities classPool at: #AuthorName put: 'TravisCI'.
Utilities classPool at: #AuthorInitials put: 'TCI'.

CodePackageFile installPackage: './Packages/BaseImageTests.pck.st' asFileEntry.

ChangeSet fileIn: './.ContinuousIntegrationScripts/TestResultConsolePrinter.st' asFileEntry.

testSuite := TestSuite new.
testCases := TestCase allSubclasses reject: [:testCase | testCase isAbstract].
testCases do: [:testCase | testCase addToSuiteFromSelectors: testSuite].

result := testSuite run.

result printReport.

exitCode := (result hasFailures or: [ result hasErrors ]) ifTrue: [ 1 ] ifFalse: [ 0 ].

Smalltalk quitPrimitive: exitCode
