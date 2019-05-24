'From Cuis 4.2 of 25 July 2013 [latest update: #2781] on 31 May 2016 at 12:06:51.48049 pm'!
'Description '!
!provides: 'SUnit XML reports' 1 8!
!requires: 'YAXO' 1 nil nil!

!TestRunner methodsFor: '*SUnit XML reports' stamp: 'jmv 6/5/2015 10:48'!
runTestsAndBuildReport
"
TestRunner new runTestsAndBuildReport edit
"
	| xmlDoc |
	xmlDoc _ self runTestsAndBuildReportXMLDoc.
	^String streamContents: [ :strm |
		xmlDoc printXMLOn: (XMLWriter on: strm) ]! !

!TestRunner methodsFor: '*SUnit XML reports' stamp: 'jmv 6/5/2015 10:47'!
runTestsAndBuildReportXMLDoc
"
TestRunner new runTestsAndBuildReport edit
"
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuite: suite.
	runSemaphore wait.
	^self xmlReport: suite! !

!TestRunner methodsFor: '*SUnit XML reports' stamp: 'jmv 6/5/2015 10:46'!
xmlReport: suite
	"See http://llg.cubic.org/docs/junit/"
	" a description of the JUnit XML format and how Jenkins parses it. See also junit.xsd "
	"Notes:
		- just one suite
		- Use current results"
	| xmlDoc suiteAttributes suiteNode |
	xmlDoc _ XMLDocument new.
	xmlDoc encoding: 'UTF-8'.
	xmlDoc version: '1.0'.

	"testsuite can appear multiple times, if contained in a testsuites element. It can also be the root element."
	suiteNode _ XMLElement named: 'testsuite'.
	suiteAttributes _ Dictionary new
		at: 'name' put: suite name;  			"Full (class) name of the test for non-aggregated testsuite documents. 
				Class name without the package for aggregated testsuites documents. Required"
		"at: 'package' put: '';" 							"Derived from testsuite/@name in the non-aggregated documents. optional"
		at: 'tests' put: suite tests size printString; 		"The total number of tests in the suite, required."
		"at: 'disabled' put: '';" 							"the total number of disabled tests in the suite. optional"
		"at: 'skipped' put: '';"					 			"The total number of skipped tests. optional"
		at: 'errors' put: result errors size printString; 	"The total number of tests in the suite that errored. An errored test is one that had an unanticipated problem,
														for example an unchecked throwable; or a problem with the implementation of the test. optional"
		at: 'failures' put: result failures size printString; 	"The total number of tests in the suite that failed. A failure is a test which the code has explicitly failed
										                               by using the mechanisms for that purpose. e.g., via an assertEquals. optional"
		"at: 'hostname' put: '';" 							"Host on which the tests were executed. 'localhost' should be used if the hostname cannot be determined. optional"
		"at: 'id' put: '0';" 									"Starts at 0 for the first testsuite and is incremented by 1 for each following testsuite"
		"at: 'time' put: '';" 					 			"Time taken (in seconds) to execute the tests in the suite. optional"
		at: 'timestamp' put: (String streamContents: [ :strm | DateAndTime nowUpToSeconds printOn: strm withLeadingSpace: false includeOffset: false ]);
												"when the test was executed in ISO 8601 format (2014-01-21T16:17:18). Timezone may not be specified. optional"
		
		yourself.
	suiteNode setAttributes: suiteAttributes.
	xmlDoc addElement: suiteNode.

	result passed do: [ :passedTestCase | | caseNode caseAttributes |
		"testcase can appear multiple times, see /testsuites/testsuite@tests"
		caseNode _ XMLElement named: 'testcase'.
		caseAttributes _ Dictionary new
			at: 'classname' put: passedTestCase class name; 	"Full class name for the class the test method is in. required"
			at: 'name' put: passedTestCase selector;  			"Name of the test method, required."
			"at: 'assertions' put: '';" 						"number of assertions in the test case. optional"
			at: 'status' put: 'passed'; 					"ok???"
			"at: 'time' put: '';" 							"Time taken (in seconds) to execute the test. optional"
			yourself.
		caseNode setAttributes: caseAttributes.
		suiteNode addElement: caseNode ].

	result failures do: [ :failedTestCase | | caseNode caseAttributes caseFailed failureAttributes |
		"testcase can appear multiple times, see /testsuites/testsuite@tests"
		caseNode _ XMLElement named: 'testcase'.
		caseAttributes _ Dictionary new
			at: 'classname' put: failedTestCase class name; 		"Full class name for the class the test method is in. required"
			at: 'name' put: failedTestCase selector;  			"Name of the test method, required."
			"at: 'assertions' put: '';" 								"number of assertions in the test case. optional"
			at: 'status' put: 'failed'; 								"ok???"
			"at: 'time' put: '';" 									"Time taken (in seconds) to execute the test. optional"
			yourself.
		caseNode setAttributes: caseAttributes.
		suiteNode addElement: caseNode.

		"Indicates that the test failed. A failure is a test which
			   the code has explicitly failed by using the mechanisms for
			   that purpose. For example via an assertEquals. Contains as
			   a text node relevant data for the failure, e.g., a stack
			   trace. optional"
		caseFailed _ XMLElement named: 'failed'.
		failureAttributes _ Dictionary new
			at: 'message' put: failedTestCase failureString; 		"The message specified in the assert."
			at: 'type' put: 'assert failure'; 						"The type of the assert. type of error that occured. e.g., if a java execption is thrown the full class name of the exception."
			yourself.
		caseFailed setAttributes: failureAttributes.
		caseNode addElement: caseFailed.
		 ].

	result errors do: [ :erroredTestCase | | caseNode caseAttributes caseErroed errorAttributes |
		"testcase can appear multiple times, see /testsuites/testsuite@tests"
		caseNode _ XMLElement named: 'testcase'.
		caseAttributes _ Dictionary new
			at: 'classname' put: erroredTestCase class name; 		"Full class name for the class the test method is in. required"
			at: 'name' put: erroredTestCase selector;  				"Name of the test method, required."
			"at: 'assertions' put: '';" 									"number of assertions in the test case. optional"
			at: 'status' put: 'error'; 									"ok???"
			"at: 'time' put: '';" 										"Time taken (in seconds) to execute the test. optional"
			yourself.
		caseNode setAttributes: caseAttributes.
		suiteNode addElement: caseNode.

		"Indicates that the test errored. An errored test is one
		           that had an unanticipated problem. For example an unchecked
		           throwable or a problem with the implementation of the
  		         test. Contains as a text node relevant data for the error,
		           for example a stack trace. optional"
		caseErroed _  XMLElement named: 'error'.
		errorAttributes _ Dictionary new
			at: 'message' put: erroredTestCase raisedError printString; 	"The error message. e.g., if a java exception is thrown, the return value of getMessage()"
			at: 'type' put: erroredTestCase raisedError class name; 		"The type of error that occured. e.g., if a java execption is thrown the full class name of the exception."
			yourself.
		caseErroed setAttributes: errorAttributes.
		caseNode addElement: caseErroed.
		].

	^xmlDoc! !

!TestRunner class methodsFor: '*SUnit XML reports' stamp: 'jmv 10/16/2015 14:48'!
buildTestRunnerApp
	"
	TestRunner buildTestRunnerApp
	"
	AppLauncher appGlobalName: self name appSelector: #runTestsAndSaveReportTo:.
	Smalltalk saveAs: 'TestRunner.image' andQuit: true clearAllClassState: false! !

!TestRunner class methodsFor: '*SUnit XML reports' stamp: 'jmv 5/31/2016 11:24'!
runTestsAndSaveReportTo: outputFileName
	| xmlDoc |
	xmlDoc _ self new runTestsAndBuildReportXMLDoc.
	outputFileName asFileEntry forceWriteStreamDo: [ :stream |
		xmlDoc printXMLOn: (XMLWriter on: stream) ].
	Smalltalk snapshot: false andQuit: true clearAllClassState: false! !
