'From Cuis 5.0 of 7 November 2016 [latest update: #3598] on 11 February 2019 at 3:04:48 pm'!

!TestCase reorganize!
('setUp/tearDown' setUp setUpResources tearDown tearDownResources)
('assertions' assert: assert:description: assert:description:resumable: assert:equals: comparingStringBetween:and: deny: deny:description: deny:description:resumable: should: should:description: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description:)
('failing' defaultFailDescription fail failWith: failureString signalFailure:)
('running' run run: runCase runCaseAsFailure)
('running - private' performTest)
('debugging' debug debugAsFailure debugAsFailureIfCanNot:)
('debugging - private' canNotDebugMethodErrorDescription ifCanNotDebugDo: openDebuggerOnFailingTestMethod signalCanNotDebugMethod)
('accessing' errored: methodForTest raisedError resources selector setTestSelector:)
('log' failureLog logFailure:)
('testing' is: isLogging isSameAs:)
('printing' printOn:)
('dependencies' addDependentToHierachy: removeDependentFromHierachy:)
!


!TestCase class reorganize!
('accessing' allTestSelectors resources sunitVersion testSelectors)
('building suites' addTestsFor:toSuite: addToSuite:fromMethods: addToSuiteFromSelectors: buildSuite buildSuiteFromAllSelectors buildSuiteFromLocalSelectors buildSuiteFromMethods: buildSuiteFromSelectors suiteClass)
('instance creation' debug: debugAsFailure:ifCanNot: run: selector:)
('testing' is: isAbstract shouldInheritSelectors)
('error descriptions' canNotDebugMethodErrorDescription)
!


!TestResource reorganize!
('accessing' description description: name name: resources)
('initialization' initialize)
('testing' isAvailable isUnavailable)
('printing' printOn:)
('setUp/tearDown' setUp signalInitializationError tearDown)
!


!TestResource class reorganize!
('accessing' current current: resources)
('testing' isAbstract isAvailable isUnavailable)
('creation' reset signalInitializationError)
!


!TestResult reorganize!
('accessing' correctCount defects errorCount errors failureCount failures passed passedCount removeFromDefectsAndAddToPassed: runCount tests)
('testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('printing' printOn:)
('running' runCase:)
!


!TestResult class reorganize!
('exceptions' error exError failure resumableFailure signalErrorWith: signalFailureWith:)
!


!TestSuite reorganize!
('accessing' addTest: addTests: defaultResources name name: resources resources: tests)
('running' run run:)
('running - private' setUpResources tearDownResources)
('dependencies' addDependentToHierachy: removeDependentFromHierachy:)
!


!TestSuite class reorganize!
('instance creation' forClass: forCompiledMethod: forMessageCategoryNamed:of:categorizedWith: forSystemCategoryNamed:using: named:)
('instance creation - private' allTestCaseClassesReferencing: allTestsSending: forClasses:named: forNoTestCaseClass: forNoTestMethod: forReferencesToClass: forTestCaseClass: forTestCaseClasses:named: forTestMethod:)
!
