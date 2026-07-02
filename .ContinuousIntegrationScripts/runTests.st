|suite result exitCode|

Preferences name: #authorInitials category: #programming value: 'TCI'.
Preferences name: #authorName category: #programming value: 'TravisCI'.

CodePackageFile installPackage: DirectoryEntry currentDirectory // 'Packages/BaseImageTests.pck.st'.

ChangeSet fileIn: DirectoryEntry currentDirectory // '.ContinuousIntegrationScripts/TestResultConsolePrinter.st'.

testSuite := TestSuite new.
testCases := TestCase allSubclasses reject: [:testCase | testCase isAbstract].
testCases do: [:testCase | testCase addToSuiteFromSelectors: testSuite].

result := testSuite run.

result printReport.

exitCode := (result hasFailures or: [ result hasErrors ]) ifTrue: [ 1 ] ifFalse: [ 0 ].

Smalltalk quitPrimitive: exitCode
