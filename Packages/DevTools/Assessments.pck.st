'From Cuis 5.0 [latest update: #3866] on 2 November 2019 at 7:43:42 pm'!
'Description Assessments is a replacement / reimplementation of the SUnit evaluation machinery, published under the MIT license (see below), with the following characteristics.

* It provides all the functionality and features of SUnit and SUnit VM (including SUnit Based Validation and SUnit Benchmarks) natively.

* For those who have made a significant investment in SUnit or SUnit VM based tests, Assessments will run them faithfully using its own evaluation framework, and without requiring any code changes whatsoever.

* A great deal of flexibility is made possible by Assessments'' design, far more than what is available with SUnit.  For example, it is possible to teach Assessments how to run tests in different testing frameworks (as exemplified by its ability to run Assessments native tests, SUnit tests, and SUnit VM tests).  The exception handling mechanism in particular is extremely flexible and robust, allowing Assessments to be easily extended without the requirement to nest exception handlers or modify Assessments code.  The evaluation and execution policies, together with the evaluation context, provide the ability to do pretty much as one pleases requiring only subclassing and composition.  As an illustration, it is straightforward to process results in any arbitrary manner as they arrive, and in fact three different ways to handle file report generation are provided by default.

The design principles behind Assessments are described in the book

	Fundamentals of Smalltalk Programming Technique, volume 1

available at http://www.lulu.com/spotlight/avsmalltalkbooks.

For a quick start, evaluate

	AssessmentsChecklistEvaluatorWindow openEvaluator

This package is internally partitioned in three main sections.  Assessments-Framework contains all the code needed to run Assessments in a headless manner.  Assessments-Tools has the checklist evaluator GUI tools.  Assessments-SelfChecks includes the code that verifies that Assessments is working properly.

1.  Assessments-Framework.

This section contains all the code necessary to run Assessments in a headless manner.  It is partitioned into the following packages.

1a. Assessments-Kernel contains the basic Assessments classes, including the counterparts to SUnit / SUnit VM''s TestCase, AbstractValidator and AbstractBenchmarkTestCase.  These are called Benchmark, Checklist, and Validator respectively.  This category also contains the base Assessments classes.  These are possibly the most interesting classes from the point of view of an Assessments user.

CheckRepository implements the base functionality of a class that provides tests.  However, the behavior provided is only on the class side.  This is on purpose to separate the management of checks from the checks themselves.  In turn this is useful for implementing the test bridges.

CheckRepository has a subclass called AbstractChecklist, which implements the base functionality of all checklists.  It contains what would correspond to the instance side of TestCase.  In this package, there are 3 visible subclasses of AbstractChecklist:

Benchmark is the counterpart to SUnit VM''s AbstractBenchmarkTestCase.

Checklist is the counterpart to SUnit''s TestCase and SUnit VM''s AbstractTestCase classes.

Validator is the counterpart to SUnit VM''s AbstractValidator.

This arrangement is employed so that the implementation of the functionality used by actual tests can be separated from the tests themselves.  In turn, this can be used to speed up test lookup considerably, see the class side of these classes and examine messages such as abstractHierarchyRoot.

AssessmentResult is the equivalent of TestResult.  You may want to examine how the atomic results are categorized internally.

Assessment is the equivalent of TestSuite.  It contains a number of checks, and is the class responsible for following the check inheritance rules for every type of check repository (although this functionality is not implemented directly but rather delegated, see class side of CheckRepository, protocol check lookup, for details).

AbstractPrerequisite and its subclasses are the equivalent of TestResource.


1b. Assessments-AtomicResults contains the objects which are stored in AssessmentResults.  These provide information as to whether checks passed, failed, ended in error, and a multitude of other bits of information.  These are categorized automatically by instances of AssessmentResult.

While in SUnit there were passes, failures and errors, the variety of tests supported by Assessments means that there will be many more result classes.  This category contains the objects used to document the result of running each individual check.


1c. Assessments-CustomChecklists contains a polymorphic enhancement to the ChecklistRepository class, and allows the creation of what in SUnit would be described as customized TestSuites.

This category implements the ability to create customized checklists.  Note that some of the subclasses also change the execution policy of the checks so that the checks can be debugged or allowed to run until they fail, as opposed to them being evaluated in a protected fashion.  In turn, this functionality is used by the evaluator tools.


1d. Assessments-EvaluationContexts contains the evaluation context object and related composite features such as those related to result handling and log file generation.

This category contains the assessment evaluation context object, which is responsible for the proper evaluation of an assessment.  Note that it is designed to be customized by subclassing, either of itself, or by subclassing AbstractEvaluationContextPolicy.  The result policies are used to collect results from the evaluation of the assessment.  There is a composite result policy which allows the daisy chaining of several policies.  This can be used to produce one or several log files, in addition to collecting the results for the evaluators.

There are 3 log file policies.  The hardened file log policy opens and closes the file for every result collected.  This is useful when the evaluation of the assessment can cause a VM crash, and allows recording of results in an accurate manner until the point of failure.  The soft file log policies implement either an open once / write as you go / close once model (FileLogAssessmentResultPolicy), or a collect all information / open once / write everything / close once model (BufferedFileLogAssessmentResultPolicy).

The other interesting policies provided are the evaluation policies.  Assessments can work by either using an exception to signal a pass, or using a fast pass execution model by which passes do not raise exceptions but rather communicate the results via the stack.  By default, evaluation contexts use the fast pass mechanism since it is considerably faster particularly for tests that execute quickly.  However, the execution policies in Assessments-ProtectedMessageSends may decide not to do this if it the resulting complexity is not worth it (such as when running benchmarks).

The execution parameters held by the assessment evaluation context are available to execution policies in case they need them for execution purposes, such as with validation.

Lazy prerequisites allow checklists to start and stop prerequisites on demand.  The LazyPrerequisitePolicy class is here to match SUnit 3.2 and above.


1e. Assessments-Exceptions contains all the exception classes and other code related to the handling of exceptions.  In general terms, each atomic result class has an associated exception class which signals its occurrence.

This category contains the polymorphic exception handling mechanism, as well as the new exception classes introduced by Assessments.  Special care should be taken not to handle debugger related exceptions at all, because doing so interferes with proper debugger behavior.  Any exception such as TerminationException should be specially handled for the same kind of reasons.

Note that exceptions are handled differently depending on the context in which they occur.  See the methods which are refined through the exception hierarchy, e.g.: occurredWhileEvaluating:inTheContextOf:.


1f. Assessments-ProtectedMessageSends contains the implementation for check objects and their execution policies, and illustrates how Assessments can learn to run tests from essentially any arbitrary test framework.

This category implements the kernel execution functionality in Assessments.

Subclasses of ProtectedMessageSend are used to send messages to objects which may be created as a result of evaluating an assessment, such as instances of TestCase or Checklist (Check), or instances of TestResource or ChecklistPrerequisite (AttemptToStartPrerequisite and AttemptToStopPrerequisite).

Checks in particular delegate much of their behavior to their associated execution policies.  The execution policies implement the actual execution behavior, down to the perform:withArguments: message.  Different execution policies implement different ways in which a check is executed.  See for example the stark contrast between the base AssessmentsCheckExecutionPolicy and AssessmentsBenchmarkCheckExecutionPolicy.  The SUnit execution policies allow Assessments to pretend to be SUnit (or SUnit VM) itself, and are selected by the test bridges.


1g. Assessments-TestBridge contains the implementation of the SUnit and SUnit VM test bridges.

This category contains the implementation of the SUnit and SUnit VM (including SUnit Benchmarks and SUnit Based Validation) bridges.  There are two parts to them.

First, the hierarchy starting at AbstractSUnitChecklistBridge is a subclass of AbstractChecklist, and as such these classes will appear in the checklist evaluator.  However, the subclasses of these are redirected via the check lookup mechanism (see checkSelectorLookupSubclasses) to instances of classes in the CheckRepositoryMetaclass hierarchy.

Second, the CheckRepositoryMetaclass hierarchy is a set of classes the instances of which pretend to be other classes.  This is useful since then the instances of CheckRepositoryMetaclass (or its subclasses) can decorate the impersonated classes with Assessments-compatible functionality without modifying the underlying classes.  Furthermore, they can pretend to be in a class hierarchy structure that either does not or simply cannot exist.  See for example how TestCase is made to appear as a subclass of SUnitVMTestCase via the SUnitVMCheckRepositoryMetaclass.

Finally, SUnitPrerequisite is the masquerading class used to wrap TestResources so that they appear to be ordinary Assessments prerequisites.


1h. The *Assessments protocols contains the additions to Cuis base classes which do not necessarily fit elsewhere.

These additions logically belong to Assessments-Framework.


2.  Assessments-SelfChecks

This sections contains Assessments'' tests, written in Assessments.  It is categorized into the following packages.


2a.  Assessments-SelfChecks-Tests contains the top level self tests for Assessments.

Note that, to avoid the appearance of failures and errors in the checklist evaluator, a two level approach is used.  These tests run other tests in the Meta tests categories, and verify that the actual results match the expectations.  This includes things like making sure that failures and errors are reported as such.  Therefore, these tests should pass without exception.


2b.  Assessments-SelfChecks-MetaBenchmarks contains tests related to Assessments Benchmarks.

These tests are the meta tests for Assessments Benchmarks.  Since they are run by the Assessments-SelfChecks-Tests category, they are marked as not visible for the evaluator tools.


2c.  Assessments-SelfChecks-MetaChecklists contains tests related to plain Assessments.

These tests are the meta tests for plain Assessments.  Since they are run by the Assessments-SelfChecks-Tests category, they are marked as not visible for the evaluator tools.


2d.  Assessments-SelfChecks-MetaValidation contains tests related to Assessments Validation.

These tests are the meta tests for Assessments Validation.  Since they are run by the Assessments-SelfChecks-Tests category, they are marked as not visible for the evaluator tools.


3.  Assessments-Tools.

This section contains Assessments'' GUI tools, categorized in the following packages.


3a.  Assessments-Tools contains the main Morphic windows and their models.  The tools, called evaluators, include an Assessment evaluator (basically the equivalent of SUnit''s test runner), and a Result evaluator.  The result evaluator comes up as a result of running assessments with the assessment evaluator.


3b.  Assessments-Tools-Support contains various classes that facilitate the operation of the evaluators.


Copyright (c) 2008 by Andres Valloud

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'!
!provides: 'Assessments' 2 36!
SystemOrganization addCategory: #'Assessments-Framework-Exceptions'!
SystemOrganization addCategory: #'Assessments-Tools'!
SystemOrganization addCategory: #'Assessments-Tools-Support'!
SystemOrganization addCategory: #'Assessments-Framework-CustomChecklists'!
SystemOrganization addCategory: #'Assessments-Framework-EvaluationContexts'!
SystemOrganization addCategory: #'Assessments-Framework-Kernel'!
SystemOrganization addCategory: #'Assessments-SelfChecks-MetaChecklists'!
SystemOrganization addCategory: #'Assessments-Framework-TestBridge'!
SystemOrganization addCategory: #'Assessments-SelfChecks-MetaBenchmarks'!
SystemOrganization addCategory: #'Assessments-SelfChecks-Tests'!
SystemOrganization addCategory: #'Assessments-SelfChecks-MetaValidation'!
SystemOrganization addCategory: #'Assessments-Framework-AtomicResults'!
SystemOrganization addCategory: #'Assessments-Framework-Portability'!
SystemOrganization addCategory: #'Assessments-Framework-ProtectedMessageSends'!


!classDefinition: #AssessmentNotification category: #'Assessments-Framework-Exceptions'!
Notification subclass: #AssessmentNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'AssessmentNotification class' category: #'Assessments-Framework-Exceptions'!
AssessmentNotification class
	instanceVariableNames: ''!

!classDefinition: #AssessmentMalfunctionNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #AssessmentMalfunctionNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'AssessmentMalfunctionNotification class' category: #'Assessments-Framework-Exceptions'!
AssessmentMalfunctionNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckAbstractFailureNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #CheckAbstractFailureNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckAbstractFailureNotification class' category: #'Assessments-Framework-Exceptions'!
CheckAbstractFailureNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckFailureNotification category: #'Assessments-Framework-Exceptions'!
CheckAbstractFailureNotification subclass: #CheckFailureNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckFailureNotification class' category: #'Assessments-Framework-Exceptions'!
CheckFailureNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckSkipNotification category: #'Assessments-Framework-Exceptions'!
CheckFailureNotification subclass: #CheckSkipNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckSkipNotification class' category: #'Assessments-Framework-Exceptions'!
CheckSkipNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckValidationFailureNotification category: #'Assessments-Framework-Exceptions'!
CheckFailureNotification subclass: #CheckValidationFailureNotification
	instanceVariableNames: 'object aspect'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckValidationFailureNotification class' category: #'Assessments-Framework-Exceptions'!
CheckValidationFailureNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckResumableFailureNotification category: #'Assessments-Framework-Exceptions'!
CheckAbstractFailureNotification subclass: #CheckResumableFailureNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckResumableFailureNotification class' category: #'Assessments-Framework-Exceptions'!
CheckResumableFailureNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkMeasurementNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #CheckBenchmarkMeasurementNotification
	instanceVariableNames: 'iterationsPerSecond prettyPrintForScenario'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckBenchmarkMeasurementNotification class' category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkMeasurementNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkAbstractExpectationNotification category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkMeasurementNotification subclass: #CheckBenchmarkAbstractExpectationNotification
	instanceVariableNames: 'expectedIterationsPerSecond'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckBenchmarkAbstractExpectationNotification class' category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkAbstractExpectationNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkExpectationFailedNotification category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkAbstractExpectationNotification subclass: #CheckBenchmarkExpectationFailedNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckBenchmarkExpectationFailedNotification class' category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkExpectationFailedNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkExpectationMetNotification category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkAbstractExpectationNotification subclass: #CheckBenchmarkExpectationMetNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckBenchmarkExpectationMetNotification class' category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkExpectationMetNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckNoticeNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #CheckNoticeNotification
	instanceVariableNames: 'notice'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckNoticeNotification class' category: #'Assessments-Framework-Exceptions'!
CheckNoticeNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckPassNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #CheckPassNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckPassNotification class' category: #'Assessments-Framework-Exceptions'!
CheckPassNotification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkPassNotification category: #'Assessments-Framework-Exceptions'!
CheckPassNotification subclass: #CheckBenchmarkPassNotification
	instanceVariableNames: 'prettyPrintForScenario'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'CheckBenchmarkPassNotification class' category: #'Assessments-Framework-Exceptions'!
CheckBenchmarkPassNotification class
	instanceVariableNames: ''!

!classDefinition: #PrerequisiteFailureNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #PrerequisiteFailureNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'PrerequisiteFailureNotification class' category: #'Assessments-Framework-Exceptions'!
PrerequisiteFailureNotification class
	instanceVariableNames: ''!

!classDefinition: #ResultAdoptionNotification category: #'Assessments-Framework-Exceptions'!
AssessmentNotification subclass: #ResultAdoptionNotification
	instanceVariableNames: 'assessmentResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Exceptions'!
!classDefinition: 'ResultAdoptionNotification class' category: #'Assessments-Framework-Exceptions'!
ResultAdoptionNotification class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsWindow category: #'Assessments-Tools'!
SystemWindow subclass: #AssessmentsWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsWindow class' category: #'Assessments-Tools'!
AssessmentsWindow class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsChecklistEvaluatorWindow category: #'Assessments-Tools'!
AssessmentsWindow subclass: #AssessmentsChecklistEvaluatorWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsChecklistEvaluatorWindow class' category: #'Assessments-Tools'!
AssessmentsChecklistEvaluatorWindow class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsResultsEvaluatorWindow category: #'Assessments-Tools'!
AssessmentsWindow subclass: #AssessmentsResultsEvaluatorWindow
	instanceVariableNames: 'resultTree'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsResultsEvaluatorWindow class' category: #'Assessments-Tools'!
AssessmentsResultsEvaluatorWindow class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsListItemWrapper category: #'Assessments-Tools-Support'!
ListItemWrapper subclass: #AssessmentsListItemWrapper
	instanceVariableNames: 'displayString contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentsListItemWrapper class' category: #'Assessments-Tools-Support'!
AssessmentsListItemWrapper class
	instanceVariableNames: ''!

!classDefinition: #AssessmentAbstractResultUIListItem category: #'Assessments-Tools-Support'!
AssessmentsListItemWrapper subclass: #AssessmentAbstractResultUIListItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentAbstractResultUIListItem class' category: #'Assessments-Tools-Support'!
AssessmentAbstractResultUIListItem class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUICheckResultListItem category: #'Assessments-Tools-Support'!
AssessmentAbstractResultUIListItem subclass: #AssessmentResultUICheckResultListItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUICheckResultListItem class' category: #'Assessments-Tools-Support'!
AssessmentResultUICheckResultListItem class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUICheckResult category: #'Assessments-Tools-Support'!
AssessmentResultUICheckResultListItem subclass: #AssessmentResultUICheckResult
	instanceVariableNames: 'checkResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUICheckResult class' category: #'Assessments-Tools-Support'!
AssessmentResultUICheckResult class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUICompositeResult category: #'Assessments-Tools-Support'!
AssessmentResultUICheckResultListItem subclass: #AssessmentResultUICompositeResult
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUICompositeResult class' category: #'Assessments-Tools-Support'!
AssessmentResultUICompositeResult class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterNode category: #'Assessments-Tools-Support'!
AssessmentResultUICompositeResult subclass: #AssessmentResultUIResultFilterNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterNode class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterNode class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilter category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterNode subclass: #AssessmentResultUIResultFilter
	instanceVariableNames: 'result'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilter class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilter class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterOthers category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilter subclass: #AssessmentResultUIResultFilterOthers
	instanceVariableNames: 'classificationTag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterOthers class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterOthers class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterPreset category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilter subclass: #AssessmentResultUIResultFilterPreset
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterPreset class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPreset class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterErrors category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPreset subclass: #AssessmentResultUIResultFilterErrors
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterErrors class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterErrors class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterFailures category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPreset subclass: #AssessmentResultUIResultFilterFailures
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterFailures class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterFailures class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterPasses category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPreset subclass: #AssessmentResultUIResultFilterPasses
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterPasses class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPasses class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterBenchmarkPasses category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterPasses subclass: #AssessmentResultUIResultFilterBenchmarkPasses
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterBenchmarkPasses class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterBenchmarkPasses class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultFilterRoot category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterNode subclass: #AssessmentResultUIResultFilterRoot
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultFilterRoot class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultFilterRoot class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResultUIResultPerClassification category: #'Assessments-Tools-Support'!
AssessmentResultUICompositeResult subclass: #AssessmentResultUIResultPerClassification
	instanceVariableNames: 'classification'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentResultUIResultPerClassification class' category: #'Assessments-Tools-Support'!
AssessmentResultUIResultPerClassification class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkResultUIListItem category: #'Assessments-Tools-Support'!
AssessmentAbstractResultUIListItem subclass: #CheckBenchmarkResultUIListItem
	instanceVariableNames: 'result'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'CheckBenchmarkResultUIListItem class' category: #'Assessments-Tools-Support'!
CheckBenchmarkResultUIListItem class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkExpectationUIListItem category: #'Assessments-Tools-Support'!
CheckBenchmarkResultUIListItem subclass: #CheckBenchmarkExpectationUIListItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'CheckBenchmarkExpectationUIListItem class' category: #'Assessments-Tools-Support'!
CheckBenchmarkExpectationUIListItem class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsChecklistWrapper category: #'Assessments-Tools-Support'!
AssessmentsListItemWrapper subclass: #AssessmentsChecklistWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools-Support'!
!classDefinition: 'AssessmentsChecklistWrapper class' category: #'Assessments-Tools-Support'!
AssessmentsChecklistWrapper class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsEvaluator category: #'Assessments-Tools'!
Object subclass: #AssessmentsEvaluator
	instanceVariableNames: 'statusBarText'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsEvaluator class' category: #'Assessments-Tools'!
AssessmentsEvaluator class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsChecklistEvaluator category: #'Assessments-Tools'!
AssessmentsEvaluator subclass: #AssessmentsChecklistEvaluator
	instanceVariableNames: 'checklists selectedChecklist checkRepositoryRoot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsChecklistEvaluator class' category: #'Assessments-Tools'!
AssessmentsChecklistEvaluator class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsResultsEvaluator category: #'Assessments-Tools'!
AssessmentsEvaluator subclass: #AssessmentsResultsEvaluator
	instanceVariableNames: 'assessment result results selectedResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Tools'!
!classDefinition: 'AssessmentsResultsEvaluator class' category: #'Assessments-Tools'!
AssessmentsResultsEvaluator class
	instanceVariableNames: ''!

!classDefinition: #AbstractCustomChecklist category: #'Assessments-Framework-CustomChecklists'!
Object subclass: #AbstractCustomChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-CustomChecklists'!
!classDefinition: 'AbstractCustomChecklist class' category: #'Assessments-Framework-CustomChecklists'!
AbstractCustomChecklist class
	instanceVariableNames: ''!

!classDefinition: #CustomChecklist category: #'Assessments-Framework-CustomChecklists'!
AbstractCustomChecklist subclass: #CustomChecklist
	instanceVariableNames: 'checks'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-CustomChecklists'!
!classDefinition: 'CustomChecklist class' category: #'Assessments-Framework-CustomChecklists'!
CustomChecklist class
	instanceVariableNames: ''!

!classDefinition: #CustomDebugChecklist category: #'Assessments-Framework-CustomChecklists'!
CustomChecklist subclass: #CustomDebugChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-CustomChecklists'!
!classDefinition: 'CustomDebugChecklist class' category: #'Assessments-Framework-CustomChecklists'!
CustomDebugChecklist class
	instanceVariableNames: ''!

!classDefinition: #CustomRunToFailureChecklist category: #'Assessments-Framework-CustomChecklists'!
CustomChecklist subclass: #CustomRunToFailureChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-CustomChecklists'!
!classDefinition: 'CustomRunToFailureChecklist class' category: #'Assessments-Framework-CustomChecklists'!
CustomRunToFailureChecklist class
	instanceVariableNames: ''!

!classDefinition: #CustomSingleThreadRunToFailureChecklist category: #'Assessments-Framework-CustomChecklists'!
CustomChecklist subclass: #CustomSingleThreadRunToFailureChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-CustomChecklists'!
!classDefinition: 'CustomSingleThreadRunToFailureChecklist class' category: #'Assessments-Framework-CustomChecklists'!
CustomSingleThreadRunToFailureChecklist class
	instanceVariableNames: ''!

!classDefinition: #AbstractEvaluationContextPolicy category: #'Assessments-Framework-EvaluationContexts'!
Object subclass: #AbstractEvaluationContextPolicy
	instanceVariableNames: 'evaluationContext'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'AbstractEvaluationContextPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
AbstractEvaluationContextPolicy class
	instanceVariableNames: ''!

!classDefinition: #AbstractAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractEvaluationContextPolicy subclass: #AbstractAssessmentResultPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'AbstractAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
AbstractAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #AbstractFileLogAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractAssessmentResultPolicy subclass: #AbstractFileLogAssessmentResultPolicy
	instanceVariableNames: 'fileName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'AbstractFileLogAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
AbstractFileLogAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #HardenedFileLogAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractFileLogAssessmentResultPolicy subclass: #HardenedFileLogAssessmentResultPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'HardenedFileLogAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
HardenedFileLogAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #SoftFileLogAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractFileLogAssessmentResultPolicy subclass: #SoftFileLogAssessmentResultPolicy
	instanceVariableNames: 'writeStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'SoftFileLogAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
SoftFileLogAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #BufferedFileLogAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
SoftFileLogAssessmentResultPolicy subclass: #BufferedFileLogAssessmentResultPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'BufferedFileLogAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
BufferedFileLogAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #FileLogAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
SoftFileLogAssessmentResultPolicy subclass: #FileLogAssessmentResultPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'FileLogAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
FileLogAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #DefaultAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractAssessmentResultPolicy subclass: #DefaultAssessmentResultPolicy
	instanceVariableNames: 'result'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'DefaultAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #CompositeAssessmentResultPolicy category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentResultPolicy subclass: #CompositeAssessmentResultPolicy
	instanceVariableNames: 'policies'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'CompositeAssessmentResultPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
CompositeAssessmentResultPolicy class
	instanceVariableNames: ''!

!classDefinition: #DefaultAssessmentEvaluationPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractEvaluationContextPolicy subclass: #DefaultAssessmentEvaluationPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'DefaultAssessmentEvaluationPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentEvaluationPolicy class
	instanceVariableNames: ''!

!classDefinition: #FastPassAssessmentEvaluationPolicy category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentEvaluationPolicy subclass: #FastPassAssessmentEvaluationPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'FastPassAssessmentEvaluationPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
FastPassAssessmentEvaluationPolicy class
	instanceVariableNames: ''!

!classDefinition: #DefaultAssessmentExecutionPolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractEvaluationContextPolicy subclass: #DefaultAssessmentExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'DefaultAssessmentExecutionPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #TimeProfilerAssessmentExecutionPolicy category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentExecutionPolicy subclass: #TimeProfilerAssessmentExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'TimeProfilerAssessmentExecutionPolicy class' category: #'Assessments-Framework-EvaluationContexts'!
TimeProfilerAssessmentExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #DefaultAssessmentPrerequisitePolicy category: #'Assessments-Framework-EvaluationContexts'!
AbstractEvaluationContextPolicy subclass: #DefaultAssessmentPrerequisitePolicy
	instanceVariableNames: 'startedPrerequisites'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'DefaultAssessmentPrerequisitePolicy class' category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentPrerequisitePolicy class
	instanceVariableNames: ''!

!classDefinition: #LazyPrerequisitePolicy category: #'Assessments-Framework-EvaluationContexts'!
DefaultAssessmentPrerequisitePolicy subclass: #LazyPrerequisitePolicy
	instanceVariableNames: 'lastCheckPrerequisites'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'LazyPrerequisitePolicy class' category: #'Assessments-Framework-EvaluationContexts'!
LazyPrerequisitePolicy class
	instanceVariableNames: ''!

!classDefinition: #AssessmentEvaluationContext category: #'Assessments-Framework-EvaluationContexts'!
Object subclass: #AssessmentEvaluationContext
	instanceVariableNames: 'assessment evaluationPolicy executionPolicy prerequisitePolicy resultPolicy executionParameters'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'AssessmentEvaluationContext class' category: #'Assessments-Framework-EvaluationContexts'!
AssessmentEvaluationContext class
	instanceVariableNames: ''!

!classDefinition: #AssessmentExecutionParameters category: #'Assessments-Framework-EvaluationContexts'!
Object subclass: #AssessmentExecutionParameters
	instanceVariableNames: 'objectToValidate originalObjectToValidate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-EvaluationContexts'!
!classDefinition: 'AssessmentExecutionParameters class' category: #'Assessments-Framework-EvaluationContexts'!
AssessmentExecutionParameters class
	instanceVariableNames: ''!

!classDefinition: #AbstractPrerequisite category: #'Assessments-Framework-Kernel'!
Object subclass: #AbstractPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'AbstractPrerequisite class' category: #'Assessments-Framework-Kernel'!
AbstractPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #ChecklistPrerequisite category: #'Assessments-Framework-Kernel'!
AbstractPrerequisite subclass: #ChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'ChecklistPrerequisite class' category: #'Assessments-Framework-Kernel'!
ChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #NonStartableChecklistPrerequisite category: #'Assessments-SelfChecks-MetaChecklists'!
ChecklistPrerequisite subclass: #NonStartableChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'NonStartableChecklistPrerequisite class' category: #'Assessments-SelfChecks-MetaChecklists'!
NonStartableChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #NonStoppableChecklistPrerequisite category: #'Assessments-SelfChecks-MetaChecklists'!
ChecklistPrerequisite subclass: #NonStoppableChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'NonStoppableChecklistPrerequisite class' category: #'Assessments-SelfChecks-MetaChecklists'!
NonStoppableChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #NotifyingChecklistPrerequisite category: #'Assessments-SelfChecks-MetaChecklists'!
ChecklistPrerequisite subclass: #NotifyingChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'NotifyingChecklistPrerequisite class' category: #'Assessments-SelfChecks-MetaChecklists'!
NotifyingChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #StartErrorChecklistPrerequisite category: #'Assessments-SelfChecks-MetaChecklists'!
ChecklistPrerequisite subclass: #StartErrorChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'StartErrorChecklistPrerequisite class' category: #'Assessments-SelfChecks-MetaChecklists'!
StartErrorChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #StopErrorChecklistPrerequisite category: #'Assessments-SelfChecks-MetaChecklists'!
ChecklistPrerequisite subclass: #StopErrorChecklistPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'StopErrorChecklistPrerequisite class' category: #'Assessments-SelfChecks-MetaChecklists'!
StopErrorChecklistPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #SUnitPrerequisite category: #'Assessments-Framework-TestBridge'!
AbstractPrerequisite subclass: #SUnitPrerequisite
	instanceVariableNames: 'testResource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitPrerequisite class' category: #'Assessments-Framework-TestBridge'!
SUnitPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #Assessment category: #'Assessments-Framework-Kernel'!
Object subclass: #Assessment
	instanceVariableNames: 'checks prerequisites'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'Assessment class' category: #'Assessments-Framework-Kernel'!
Assessment class
	instanceVariableNames: ''!

!classDefinition: #AssessmentResult category: #'Assessments-Framework-Kernel'!
Object subclass: #AssessmentResult
	instanceVariableNames: 'results'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'AssessmentResult class' category: #'Assessments-Framework-Kernel'!
AssessmentResult class
	instanceVariableNames: ''!

!classDefinition: #CheckRepository category: #'Assessments-Framework-Kernel'!
Object subclass: #CheckRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'CheckRepository class' category: #'Assessments-Framework-Kernel'!
CheckRepository class
	instanceVariableNames: ''!

!classDefinition: #AbstractChecklist category: #'Assessments-Framework-Kernel'!
CheckRepository subclass: #AbstractChecklist
	instanceVariableNames: 'prettyPrint'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'AbstractChecklist class' category: #'Assessments-Framework-Kernel'!
AbstractChecklist class
	instanceVariableNames: ''!

!classDefinition: #Benchmark category: #'Assessments-Framework-Kernel'!
AbstractChecklist subclass: #Benchmark
	instanceVariableNames: 'prettyPrintForScenario'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'Benchmark class' category: #'Assessments-Framework-Kernel'!
Benchmark class
	instanceVariableNames: ''!

!classDefinition: #BenchmarkMetaSelfChecklist category: #'Assessments-SelfChecks-MetaBenchmarks'!
Benchmark subclass: #BenchmarkMetaSelfChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaBenchmarks'!
!classDefinition: 'BenchmarkMetaSelfChecklist class' category: #'Assessments-SelfChecks-MetaBenchmarks'!
BenchmarkMetaSelfChecklist class
	instanceVariableNames: ''!

!classDefinition: #BenchmarkChecklist category: #'Assessments-SelfChecks-MetaBenchmarks'!
BenchmarkMetaSelfChecklist subclass: #BenchmarkChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaBenchmarks'!
!classDefinition: 'BenchmarkChecklist class' category: #'Assessments-SelfChecks-MetaBenchmarks'!
BenchmarkChecklist class
	instanceVariableNames: ''!

!classDefinition: #Checklist category: #'Assessments-Framework-Kernel'!
AbstractChecklist subclass: #Checklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'Checklist class' category: #'Assessments-Framework-Kernel'!
Checklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsMetaSelfChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
Checklist subclass: #AssessmentsMetaSelfChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsMetaSelfChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsAcceptChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsAcceptChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsAcceptChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsAcceptChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsResumablyAcceptChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsAcceptChecklist subclass: #AssessmentsResumablyAcceptChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsResumablyAcceptChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsResumablyAcceptChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsAcceptValueOfChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsAcceptValueOfChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsAcceptValueOfChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsAcceptValueOfChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsCheckExecutionSetUpChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsCheckExecutionSetUpChecklist
	instanceVariableNames: 'aboutToEvaluateCheckSent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsCheckExecutionSetUpChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsCheckExecutionSetUpChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsCheckExecutionSetUpFailureChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsCheckExecutionSetUpChecklist subclass: #AssessmentsCheckExecutionSetUpFailureChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsCheckExecutionSetUpFailureChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsCheckExecutionSetUpFailureChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsCheckExecutionTearDownChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsCheckExecutionSetUpChecklist subclass: #AssessmentsCheckExecutionTearDownChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsCheckExecutionTearDownChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsCheckExecutionTearDownChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsDefaultPrettyPrintChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsDefaultPrettyPrintChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsDefaultPrettyPrintChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsDefaultPrettyPrintChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsEvaluationOfDoesNotResultInChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsEvaluationOfDoesNotResultInChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsEvaluationOfDoesNotResultInChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsEvaluationOfDoesNotResultInChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsEvaluationOfResultsInChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsEvaluationOfResultsInChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsEvaluationOfResultsInChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsEvaluationOfResultsInChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNoOpCheckChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsNoOpCheckChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsNoOpCheckChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsNoOpCheckChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNoticeChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsNoticeChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsNoticeChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsNoticeChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNotifyingPrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsNotifyingPrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsNotifyingPrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsNotifyingPrerequisiteChecklist class
	instanceVariableNames: 'prerequisiteStarted'!

!classDefinition: #AssessmentsPrettyPrintChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsPrettyPrintChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsPrettyPrintChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsPrettyPrintChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsRejectChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsRejectChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsRejectChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsRejectChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsResumablyRejectChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsRejectChecklist subclass: #AssessmentsResumablyRejectChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsResumablyRejectChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsResumablyRejectChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsRejectValueOfChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsRejectValueOfChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsRejectValueOfChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsRejectValueOfChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsSimplePrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsMetaSelfChecklist subclass: #AssessmentsSimplePrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsSimplePrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsSimplePrerequisiteChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNonStartablePrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsSimplePrerequisiteChecklist subclass: #AssessmentsNonStartablePrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsNonStartablePrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsNonStartablePrerequisiteChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNonStoppablePrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsSimplePrerequisiteChecklist subclass: #AssessmentsNonStoppablePrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsNonStoppablePrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsNonStoppablePrerequisiteChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsStartErrorPrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsSimplePrerequisiteChecklist subclass: #AssessmentsStartErrorPrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsStartErrorPrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsStartErrorPrerequisiteChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsStopErrorPrerequisiteChecklist category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsSimplePrerequisiteChecklist subclass: #AssessmentsStopErrorPrerequisiteChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaChecklists'!
!classDefinition: 'AssessmentsStopErrorPrerequisiteChecklist class' category: #'Assessments-SelfChecks-MetaChecklists'!
AssessmentsStopErrorPrerequisiteChecklist class
	instanceVariableNames: ''!

!classDefinition: #AbstractSelfVerificationChecklist category: #'Assessments-SelfChecks-Tests'!
Checklist subclass: #AbstractSelfVerificationChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AbstractSelfVerificationChecklist class' category: #'Assessments-SelfChecks-Tests'!
AbstractSelfVerificationChecklist class
	instanceVariableNames: ''!

!classDefinition: #AbstractAssessmentsSelfVerificationChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractSelfVerificationChecklist subclass: #AbstractAssessmentsSelfVerificationChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AbstractAssessmentsSelfVerificationChecklist class' category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsCodeMetricsChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #AssessmentsCodeMetricsChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsCodeMetricsChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsCodeMetricsChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsDefaultEvaluationContextChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #AssessmentsDefaultEvaluationContextChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsDefaultEvaluationContextChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsDefaultEvaluationContextChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsFastPassEvaluationPolicyChecklist category: #'Assessments-SelfChecks-Tests'!
AssessmentsDefaultEvaluationContextChecklist subclass: #AssessmentsFastPassEvaluationPolicyChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsFastPassEvaluationPolicyChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsFastPassEvaluationPolicyChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsManualResultAdoptionChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #AssessmentsManualResultAdoptionChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsManualResultAdoptionChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsManualResultAdoptionChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsAutomaticResultAdoptionChecklist category: #'Assessments-SelfChecks-Tests'!
AssessmentsManualResultAdoptionChecklist subclass: #AssessmentsAutomaticResultAdoptionChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsAutomaticResultAdoptionChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsAutomaticResultAdoptionChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsSelfVerificationChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #AssessmentsSelfVerificationChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsSelfVerificationChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsSelfVerificationChecklist class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsServicesChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #AssessmentsServicesChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'AssessmentsServicesChecklist class' category: #'Assessments-SelfChecks-Tests'!
AssessmentsServicesChecklist class
	instanceVariableNames: ''!

!classDefinition: #ValidationSelfVerificationChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractAssessmentsSelfVerificationChecklist subclass: #ValidationSelfVerificationChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'ValidationSelfVerificationChecklist class' category: #'Assessments-SelfChecks-Tests'!
ValidationSelfVerificationChecklist class
	instanceVariableNames: ''!

!classDefinition: #BenchmarksSelfVerificationChecklist category: #'Assessments-SelfChecks-Tests'!
AbstractSelfVerificationChecklist subclass: #BenchmarksSelfVerificationChecklist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-Tests'!
!classDefinition: 'BenchmarksSelfVerificationChecklist class' category: #'Assessments-SelfChecks-Tests'!
BenchmarksSelfVerificationChecklist class
	instanceVariableNames: ''!

!classDefinition: #Validator category: #'Assessments-Framework-Kernel'!
AbstractChecklist subclass: #Validator
	instanceVariableNames: 'object originalObject aspect reportFailuresFor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Kernel'!
!classDefinition: 'Validator class' category: #'Assessments-Framework-Kernel'!
Validator class
	instanceVariableNames: ''!

!classDefinition: #ValidationMetaSelfValidator category: #'Assessments-SelfChecks-MetaValidation'!
Validator subclass: #ValidationMetaSelfValidator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaValidation'!
!classDefinition: 'ValidationMetaSelfValidator class' category: #'Assessments-SelfChecks-MetaValidation'!
ValidationMetaSelfValidator class
	instanceVariableNames: ''!

!classDefinition: #ValidationValidator category: #'Assessments-SelfChecks-MetaValidation'!
ValidationMetaSelfValidator subclass: #ValidationValidator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-SelfChecks-MetaValidation'!
!classDefinition: 'ValidationValidator class' category: #'Assessments-SelfChecks-MetaValidation'!
ValidationValidator class
	instanceVariableNames: ''!

!classDefinition: #AbstractSUnitChecklistBridge category: #'Assessments-Framework-TestBridge'!
AbstractChecklist subclass: #AbstractSUnitChecklistBridge
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'AbstractSUnitChecklistBridge class' category: #'Assessments-Framework-TestBridge'!
AbstractSUnitChecklistBridge class
	instanceVariableNames: ''!

!classDefinition: #SUnitTestCase category: #'Assessments-Framework-TestBridge'!
AbstractSUnitChecklistBridge subclass: #SUnitTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitTestCase class
	instanceVariableNames: ''!

!classDefinition: #SUnitTooTestCase category: #'Assessments-Framework-TestBridge'!
AbstractSUnitChecklistBridge subclass: #SUnitTooTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitTooTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitTooTestCase class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMAbstractTestCase category: #'Assessments-Framework-TestBridge'!
AbstractSUnitChecklistBridge subclass: #SUnitVMAbstractTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMAbstractTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractTestCase class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMBenchmarkTestCase category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractTestCase subclass: #SUnitVMBenchmarkTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMBenchmarkTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitVMBenchmarkTestCase class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMTestCase category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractTestCase subclass: #SUnitVMTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitVMTestCase class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMValidatorTestCase category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractTestCase subclass: #SUnitVMValidatorTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMValidatorTestCase class' category: #'Assessments-Framework-TestBridge'!
SUnitVMValidatorTestCase class
	instanceVariableNames: ''!

!classDefinition: #CheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
Object subclass: #CheckRepositoryMetaclass
	instanceVariableNames: 'classToImpersonate superclass subclasses'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'CheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
CheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #AbstractSUnitCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
CheckRepositoryMetaclass subclass: #AbstractSUnitCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'AbstractSUnitCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
AbstractSUnitCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
AbstractSUnitCheckRepositoryMetaclass subclass: #SUnitCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitCheckRepositoryRootMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitCheckRepositoryMetaclass subclass: #SUnitCheckRepositoryRootMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitCheckRepositoryRootMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitCheckRepositoryRootMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMAbstractCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitCheckRepositoryMetaclass subclass: #SUnitVMAbstractCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMAbstractCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMBenchmarkCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractCheckRepositoryMetaclass subclass: #SUnitVMBenchmarkCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMBenchmarkCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitVMBenchmarkCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractCheckRepositoryMetaclass subclass: #SUnitVMCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitVMCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMValidatorCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitVMAbstractCheckRepositoryMetaclass subclass: #SUnitVMValidatorCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitVMValidatorCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitVMValidatorCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitTooCheckRepositoryMetaclass category: #'Assessments-Framework-TestBridge'!
AbstractSUnitCheckRepositoryMetaclass subclass: #SUnitTooCheckRepositoryMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitTooCheckRepositoryMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitTooCheckRepositoryMetaclass class
	instanceVariableNames: ''!

!classDefinition: #SUnitTooCheckRepositoryRootMetaclass category: #'Assessments-Framework-TestBridge'!
SUnitTooCheckRepositoryMetaclass subclass: #SUnitTooCheckRepositoryRootMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-TestBridge'!
!classDefinition: 'SUnitTooCheckRepositoryRootMetaclass class' category: #'Assessments-Framework-TestBridge'!
SUnitTooCheckRepositoryRootMetaclass class
	instanceVariableNames: ''!

!classDefinition: #AbstractResult category: #'Assessments-Framework-AtomicResults'!
Object subclass: #AbstractResult
	instanceVariableNames: 'prettyPrint'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'AbstractResult class' category: #'Assessments-Framework-AtomicResults'!
AbstractResult class
	instanceVariableNames: ''!

!classDefinition: #CheckResult category: #'Assessments-Framework-AtomicResults'!
AbstractResult subclass: #CheckResult
	instanceVariableNames: 'check'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckResult class' category: #'Assessments-Framework-AtomicResults'!
CheckResult class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkPass category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckBenchmarkPass
	instanceVariableNames: 'prettyPrintForScenario'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckBenchmarkPass class' category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkPass class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkResult category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckBenchmarkResult
	instanceVariableNames: 'iterationsPerSecond prettyPrintForScenario'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckBenchmarkResult class' category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkResult class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkAbstractExpectationNotice category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkResult subclass: #CheckBenchmarkAbstractExpectationNotice
	instanceVariableNames: 'expectedIterationsPerSecond'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckBenchmarkAbstractExpectationNotice class' category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkAbstractExpectationNotice class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkExpectationFailure category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkAbstractExpectationNotice subclass: #CheckBenchmarkExpectationFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckBenchmarkExpectationFailure class' category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkExpectationFailure class
	instanceVariableNames: ''!

!classDefinition: #CheckBenchmarkExpectationPass category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkAbstractExpectationNotice subclass: #CheckBenchmarkExpectationPass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckBenchmarkExpectationPass class' category: #'Assessments-Framework-AtomicResults'!
CheckBenchmarkExpectationPass class
	instanceVariableNames: ''!

!classDefinition: #CheckError category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckError class' category: #'Assessments-Framework-AtomicResults'!
CheckError class
	instanceVariableNames: ''!

!classDefinition: #CheckFailure category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckFailure class' category: #'Assessments-Framework-AtomicResults'!
CheckFailure class
	instanceVariableNames: ''!

!classDefinition: #CheckValidationFailure category: #'Assessments-Framework-AtomicResults'!
CheckFailure subclass: #CheckValidationFailure
	instanceVariableNames: 'object aspect'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckValidationFailure class' category: #'Assessments-Framework-AtomicResults'!
CheckValidationFailure class
	instanceVariableNames: ''!

!classDefinition: #CheckNotice category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckNotice
	instanceVariableNames: 'notice'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckNotice class' category: #'Assessments-Framework-AtomicResults'!
CheckNotice class
	instanceVariableNames: ''!

!classDefinition: #CheckPass category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckPass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckPass class' category: #'Assessments-Framework-AtomicResults'!
CheckPass class
	instanceVariableNames: ''!

!classDefinition: #CheckSkip category: #'Assessments-Framework-AtomicResults'!
CheckResult subclass: #CheckSkip
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'CheckSkip class' category: #'Assessments-Framework-AtomicResults'!
CheckSkip class
	instanceVariableNames: ''!

!classDefinition: #PrerequisiteResult category: #'Assessments-Framework-AtomicResults'!
AbstractResult subclass: #PrerequisiteResult
	instanceVariableNames: 'prerequisite'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'PrerequisiteResult class' category: #'Assessments-Framework-AtomicResults'!
PrerequisiteResult class
	instanceVariableNames: ''!

!classDefinition: #PrerequisiteStartFailure category: #'Assessments-Framework-AtomicResults'!
PrerequisiteResult subclass: #PrerequisiteStartFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'PrerequisiteStartFailure class' category: #'Assessments-Framework-AtomicResults'!
PrerequisiteStartFailure class
	instanceVariableNames: ''!

!classDefinition: #PrerequisiteStopFailure category: #'Assessments-Framework-AtomicResults'!
PrerequisiteResult subclass: #PrerequisiteStopFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-AtomicResults'!
!classDefinition: 'PrerequisiteStopFailure class' category: #'Assessments-Framework-AtomicResults'!
PrerequisiteStopFailure class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsPortabilityService category: #'Assessments-Framework-Portability'!
Object subclass: #AssessmentsPortabilityService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsPortabilityService class' category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService class
	instanceVariableNames: 'current'!

!classDefinition: #AssessmentsClassResolver category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsClassResolver
	instanceVariableNames: 'sunitTestCaseRoot sunitFailureNotification sunitResumableFailureNotification sunitTestSkipNotification sunitTooTestCaseRoot sunitTooFailureNotification sunitVMTestCaseRootForVisibility sunitVMBenchmarkExpectationNotification sunitVMBenchmarkMeasurementNotification sunitVMBenchmarkTestCaseRoot sunitVMNoticeNotification sunitVMValidationFailureNotification sunitVMValidationTestCaseRoot selfTestsTestObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsClassResolver class' category: #'Assessments-Framework-Portability'!
AssessmentsClassResolver class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsDateAndTime category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsDateAndTime
	instanceVariableNames: 'dateAndTimeClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsDateAndTime class' category: #'Assessments-Framework-Portability'!
AssessmentsDateAndTime class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsFileDirectory category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsFileDirectory class' category: #'Assessments-Framework-Portability'!
AssessmentsFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsGC category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsGC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsGC class' category: #'Assessments-Framework-Portability'!
AssessmentsGC class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsNumberPrinter category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsNumberPrinter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsNumberPrinter class' category: #'Assessments-Framework-Portability'!
AssessmentsNumberPrinter class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsProcessScheduler category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsProcessScheduler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsProcessScheduler class' category: #'Assessments-Framework-Portability'!
AssessmentsProcessScheduler class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsProfiler category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsProfiler
	instanceVariableNames: 'profiler'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsProfiler class' category: #'Assessments-Framework-Portability'!
AssessmentsProfiler class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsSystemOrganization category: #'Assessments-Framework-Portability'!
AssessmentsPortabilityService subclass: #AssessmentsSystemOrganization
	instanceVariableNames: 'profiler'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-Portability'!
!classDefinition: 'AssessmentsSystemOrganization class' category: #'Assessments-Framework-Portability'!
AssessmentsSystemOrganization class
	instanceVariableNames: ''!

!classDefinition: #CheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
Object subclass: #CheckExecutionPolicy
	instanceVariableNames: 'exceptionsToTrap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'CheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
CheckExecutionPolicy class
	instanceVariableNames: 'current'!

!classDefinition: #AbstractSUnitCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
CheckExecutionPolicy subclass: #AbstractSUnitCheckExecutionPolicy
	instanceVariableNames: 'passNotification isAcceptable isUnacceptable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AbstractSUnitCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractSUnitCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #SUnitCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractSUnitCheckExecutionPolicy subclass: #SUnitCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SUnitCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitCheckExecutionPolicy subclass: #SUnitVMCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SUnitVMCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitVMCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMBenchmarkExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitVMCheckExecutionPolicy subclass: #SUnitVMBenchmarkExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SUnitVMBenchmarkExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitVMBenchmarkExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #SUnitVMValidationExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitVMCheckExecutionPolicy subclass: #SUnitVMValidationExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SUnitVMValidationExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitVMValidationExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #SUnitTooCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractSUnitCheckExecutionPolicy subclass: #SUnitTooCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SUnitTooCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
SUnitTooCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsBenchmarkCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
CheckExecutionPolicy subclass: #AssessmentsBenchmarkCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AssessmentsBenchmarkCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
AssessmentsBenchmarkCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
CheckExecutionPolicy subclass: #AssessmentsCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AssessmentsCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
AssessmentsCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #AssessmentsValidationCheckExecutionPolicy category: #'Assessments-Framework-ProtectedMessageSends'!
AssessmentsCheckExecutionPolicy subclass: #AssessmentsValidationCheckExecutionPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AssessmentsValidationCheckExecutionPolicy class' category: #'Assessments-Framework-ProtectedMessageSends'!
AssessmentsValidationCheckExecutionPolicy class
	instanceVariableNames: ''!

!classDefinition: #ProtectedMessageSend category: #'Assessments-Framework-ProtectedMessageSends'!
Object subclass: #ProtectedMessageSend
	instanceVariableNames: 'receiver selector arguments executionPolicy'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'ProtectedMessageSend class' category: #'Assessments-Framework-ProtectedMessageSends'!
ProtectedMessageSend class
	instanceVariableNames: ''!

!classDefinition: #AttemptToStartPrerequisite category: #'Assessments-Framework-ProtectedMessageSends'!
ProtectedMessageSend subclass: #AttemptToStartPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AttemptToStartPrerequisite class' category: #'Assessments-Framework-ProtectedMessageSends'!
AttemptToStartPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #AttemptToStopPrerequisite category: #'Assessments-Framework-ProtectedMessageSends'!
AttemptToStartPrerequisite subclass: #AttemptToStopPrerequisite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AttemptToStopPrerequisite class' category: #'Assessments-Framework-ProtectedMessageSends'!
AttemptToStopPrerequisite class
	instanceVariableNames: ''!

!classDefinition: #Check category: #'Assessments-Framework-ProtectedMessageSends'!
ProtectedMessageSend subclass: #Check
	instanceVariableNames: 'actualReceiver'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'Check class' category: #'Assessments-Framework-ProtectedMessageSends'!
Check class
	instanceVariableNames: ''!

!classDefinition: #AbstractDebugCheck category: #'Assessments-Framework-ProtectedMessageSends'!
Check subclass: #AbstractDebugCheck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'AbstractDebugCheck class' category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractDebugCheck class
	instanceVariableNames: ''!

!classDefinition: #DebugCheck category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractDebugCheck subclass: #DebugCheck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'DebugCheck class' category: #'Assessments-Framework-ProtectedMessageSends'!
DebugCheck class
	instanceVariableNames: ''!

!classDefinition: #RunToFailureCheck category: #'Assessments-Framework-ProtectedMessageSends'!
AbstractDebugCheck subclass: #RunToFailureCheck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'RunToFailureCheck class' category: #'Assessments-Framework-ProtectedMessageSends'!
RunToFailureCheck class
	instanceVariableNames: ''!

!classDefinition: #SingleThreadRunToFailureCheck category: #'Assessments-Framework-ProtectedMessageSends'!
Check subclass: #SingleThreadRunToFailureCheck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Assessments-Framework-ProtectedMessageSends'!
!classDefinition: 'SingleThreadRunToFailureCheck class' category: #'Assessments-Framework-ProtectedMessageSends'!
SingleThreadRunToFailureCheck class
	instanceVariableNames: ''!


!AssessmentNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentMalfunctionNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckAbstractFailureNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckFailureNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckSkipNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckValidationFailureNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckResumableFailureNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkMeasurementNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkAbstractExpectationNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkExpectationFailedNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkExpectationMetNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckNoticeNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckPassNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkPassNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!PrerequisiteFailureNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ResultAdoptionNotification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsWindow commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsChecklistEvaluatorWindow commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsResultsEvaluatorWindow commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsListItemWrapper commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentAbstractResultUIListItem commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUICheckResultListItem commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUICheckResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUICompositeResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterNode commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilter commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterOthers commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterPreset commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterErrors commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterFailures commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterPasses commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterBenchmarkPasses commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultFilterRoot commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResultUIResultPerClassification commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkResultUIListItem commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkExpectationUIListItem commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsChecklistWrapper commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsEvaluator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsChecklistEvaluator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsResultsEvaluator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractCustomChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CustomChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CustomDebugChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CustomRunToFailureChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CustomSingleThreadRunToFailureChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractEvaluationContextPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractFileLogAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!HardenedFileLogAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SoftFileLogAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!BufferedFileLogAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!FileLogAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!DefaultAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CompositeAssessmentResultPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!DefaultAssessmentEvaluationPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!FastPassAssessmentEvaluationPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!DefaultAssessmentExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!TimeProfilerAssessmentExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!DefaultAssessmentPrerequisitePolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!LazyPrerequisitePolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentEvaluationContext commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentExecutionParameters commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!NonStartableChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!NonStoppableChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!NotifyingChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!StartErrorChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!StopErrorChecklistPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!Assessment commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckRepository commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!Benchmark commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!BenchmarkMetaSelfChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!BenchmarkChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!Checklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsMetaSelfChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsAcceptChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsResumablyAcceptChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsAcceptValueOfChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsCheckExecutionSetUpChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsCheckExecutionSetUpFailureChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsCheckExecutionTearDownChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsDefaultPrettyPrintChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsEvaluationOfDoesNotResultInChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsEvaluationOfResultsInChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsNoOpCheckChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsNoticeChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsNotifyingPrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsPrettyPrintChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsRejectChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsResumablyRejectChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsRejectValueOfChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsSimplePrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsNonStartablePrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsNonStoppablePrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsStartErrorPrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsStopErrorPrerequisiteChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractSelfVerificationChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractAssessmentsSelfVerificationChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsCodeMetricsChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsDefaultEvaluationContextChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsFastPassEvaluationPolicyChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsManualResultAdoptionChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsAutomaticResultAdoptionChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsSelfVerificationChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsServicesChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ValidationSelfVerificationChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!BenchmarksSelfVerificationChecklist commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!Validator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ValidationMetaSelfValidator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ValidationValidator commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractSUnitChecklistBridge commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitTooTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMAbstractTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMBenchmarkTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMValidatorTestCase commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractSUnitCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitCheckRepositoryRootMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMAbstractCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMBenchmarkCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMValidatorCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitTooCheckRepositoryMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitTooCheckRepositoryRootMetaclass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkPass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkAbstractExpectationNotice commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkExpectationFailure commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckBenchmarkExpectationPass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckError commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckFailure commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckValidationFailure commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckNotice commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckPass commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckSkip commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!PrerequisiteResult commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!PrerequisiteStartFailure commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!PrerequisiteStopFailure commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsPortabilityService commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsClassResolver commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsFileDirectory commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsProfiler commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!CheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractSUnitCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMBenchmarkExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitVMValidationExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SUnitTooCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsBenchmarkCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsValidationCheckExecutionPolicy commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!ProtectedMessageSend commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AttemptToStartPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AttemptToStopPrerequisite commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!Check commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AbstractDebugCheck commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!DebugCheck commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!RunToFailureCheck commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!SingleThreadRunToFailureCheck commentStamp: '<historical>' prior: 0!
See package comments for more information.!

!AssessmentsListItemWrapper methodsFor: 'printing' stamp: 'SqR 9/7/2015 16:48'!
printOn: aStream

	aStream nextPutAll: self displayString! !

!AbstractCustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
superclass

	self shouldNotImplement! !

!AbstractPrerequisite methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	aStream
		nextPutAll: self prettyPrint;
		nextPut: $(;
		print: self class;
		nextPut: $)! !

!SUnitPrerequisite methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	self class printOn: aStream.
	aStream nextPut: $(.
	self testResource printOn: aStream.
	aStream nextPut: $)! !

!Assessment methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	self checks
		do: [:each | each printOn: aStream]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!AssessmentResult methodsFor: 'printing' stamp: 'sqr 10/4/2018 19:49:15'!
printOn: aStream

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	(self results keys asSortedCollection: [:x :y | x name < y name])
		do:
			[:eachResultTag |
				(self results at: eachResultTag)
					do: [:eachResult | eachResult printOn: aStream]
					separatedBy: [aStream newLine]
			]
		separatedBy: [aStream newLine; newLine].
	aStream nextPut: $)! !

!CheckRepository class methodsFor: 'selector lookup'!
selectorsDo: aBlock
	"This is ~3x faster than self selectors do:"

	self selectorsAndMethodsDo: [:eachSelector :eachMethod | aBlock value: eachSelector]! !

!CheckRepositoryMetaclass methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
name

	self classToImpersonate isNil ifTrue: [^self class name].
	^self classToImpersonate name! !

!CheckRepositoryMetaclass methodsFor: 'browsing' stamp: 'SqR 3/8/2015 18:10'!
classForBrowsing

	^self classToImpersonate! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
subclasses

	^subclasses! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
superclass

	^superclass! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
superclass: anObject

	superclass := anObject! !

!CheckRepositoryMetaclass methodsFor: 'classification' stamp: 'sqr 6/26/2019 09:35:13'!
category

	^self classToImpersonate category! !

!CheckRepositoryMetaclass methodsFor: 'classification' stamp: 'sqr 6/26/2019 09:34:42'!
isMeta

	^self classToImpersonate isMeta! !

!CheckRepositoryMetaclass methodsFor: 'selector lookup' stamp: 'SqR 3/8/2015 18:10'!
selectorsDo: aBlock
	"This is ~3x faster than self selectors do:"

	self classToImpersonate selectorsAndMethodsDo:
		[:eachSelector :eachMethod | aBlock value: eachSelector]! !

!CheckRepositoryMetaclass methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	self classToImpersonate printOn: aStream.
	aStream nextPut: $*! !

!AbstractResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	self printResultDetailsOn: aStream.
	aStream nextPut: $).
	self printPrettyPrintDetailsOn: aStream! !

!ProtectedMessageSend methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printOn: aStream

	aStream
		nextPutAll: self receiverPrintString;
		nextPutAll: '>>'.
	self printBrieflyOn: aStream! !

!AssessmentNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
handlerActionForEvaluation

	self resume! !

!AssessmentNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
handlerActionForPrerequisiteEvaluation

	self return: false! !

!AssessmentNotification methodsFor: 'assessments' stamp: 'SqR 2/22/2015 18:58'!
messageTextForHandling

	^self messageText! !

!AssessmentMalfunctionNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
defaultAction

	^self noHandler! !

!AssessmentMalfunctionNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
handlerActionForEvaluation

	self pass! !

!AssessmentMalfunctionNotification methodsFor: 'assessments' stamp: 'SqR 3/24/2015 19:02'!
preventsCheckSuccess

	^true! !

!AssessmentMalfunctionNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
resignalAsAssessmentMalfunctionNotification

	self pass! !

!CheckAbstractFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
defaultAction

	^self noHandler! !

!CheckAbstractFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	| failure |
	failure := self resultFor: aCheck.
	anEvaluationContext addResult: failure.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!CheckAbstractFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/24/2015 19:02'!
preventsCheckSuccess

	^true! !

!CheckAbstractFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
resultFor: aCheck

	| failure |
	failure := aCheck newCheckFailure.
	failure prettyPrint: self messageTextForHandling.
	^failure! !

!CheckFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
handlerActionForEvaluation

	self return! !

!CheckSkipNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
resultFor: aCheck

	| failure |
	failure := aCheck newCheckSkip.
	failure prettyPrint: self messageTextForHandling.
	^failure! !

!CheckValidationFailureNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect

	^aspect! !

!CheckValidationFailureNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
object

	^object! !

!CheckValidationFailureNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect: anObject

	aspect := anObject! !

!CheckValidationFailureNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
object: anObject

	object := anObject! !

!CheckValidationFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
resultFor: aCheck

	| failure |
	failure := aCheck newCheckValidationFailure.
	failure prettyPrint: self messageTextForHandling.
	failure object: self object.
	failure aspect: self aspect.
	^failure! !

!CheckBenchmarkMeasurementNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
iterationsPerSecond

	^iterationsPerSecond! !

!CheckBenchmarkMeasurementNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario

	^prettyPrintForScenario! !

!CheckBenchmarkMeasurementNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
iterationsPerSecond: anObject

	iterationsPerSecond := anObject! !

!CheckBenchmarkMeasurementNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario: anObject

	prettyPrintForScenario := anObject! !

!CheckBenchmarkMeasurementNotification methodsFor: 'private - assessments' stamp: 'SqR 3/8/2015 18:10'!
newResultFor: aCheck

	^aCheck newCheckBenchmarkResult! !

!CheckBenchmarkMeasurementNotification methodsFor: 'private - assessments' stamp: 'SqR 3/8/2015 18:10'!
resultFor: aCheck

	| newResult |
	newResult := self newResultFor: aCheck.
	newResult prettyPrint: self messageTextForHandling.
	newResult prettyPrintForScenario: self prettyPrintForScenario.
	newResult iterationsPerSecond: self iterationsPerSecond.
	^newResult! !

!CheckBenchmarkMeasurementNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	| newResult |
	newResult := self resultFor: aCheck.
	anEvaluationContext addResult: newResult.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!CheckBenchmarkAbstractExpectationNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
expectedIterationsPerSecond

	^expectedIterationsPerSecond! !

!CheckBenchmarkAbstractExpectationNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
expectedIterationsPerSecond: anObject

	expectedIterationsPerSecond := anObject! !

!CheckBenchmarkAbstractExpectationNotification methodsFor: 'private - assessments' stamp: 'SqR 3/8/2015 18:10'!
resultFor: aCheck

	| newResult |
	newResult := super resultFor: aCheck.
	newResult expectedIterationsPerSecond: self expectedIterationsPerSecond.
	^newResult! !

!CheckBenchmarkExpectationFailedNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
newResultFor: aCheck

	^aCheck newCheckBenchmarkExpectationFailed! !

!CheckBenchmarkExpectationMetNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
newResultFor: aCheck

	^aCheck newCheckBenchmarkExpectationMet! !

!CheckNoticeNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
notice

	^notice! !

!CheckNoticeNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
notice: anObject

	notice := anObject! !

!CheckNoticeNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
noticeFor: aCheck

	^aCheck newCheckNotice! !

!CheckNoticeNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	| newNotice |
	newNotice := self noticeFor: aCheck.
	newNotice notice: self notice.
	newNotice prettyPrint: self messageTextForHandling.
	anEvaluationContext addResult: newNotice.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!CheckPassNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	self recordPassFor: aCheck in: anEvaluationContext.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!CheckPassNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
recordPassFor: aCheck in: anEvaluationContext

	| pass |
	pass := aCheck newCheckPass.
	pass prettyPrint: self messageTextForHandling.
	anEvaluationContext addResult: pass! !

!CheckBenchmarkPassNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario

	^prettyPrintForScenario! !

!CheckBenchmarkPassNotification methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario: anObject

	prettyPrintForScenario := anObject! !

!CheckBenchmarkPassNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
recordPassFor: aCheck in: anEvaluationContext

	| pass |
	pass := aCheck newCheckBenchmarkPass.
	pass prettyPrint: self messageTextForHandling.
	pass prettyPrintForScenario: self prettyPrintForScenario.
	anEvaluationContext addResult: pass! !

!PrerequisiteFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
handlerActionForPrerequisiteEvaluation

	self return: false! !

!PrerequisiteFailureNotification methodsFor: 'assessments' stamp: 'SqR 3/24/2015 19:02'!
preventsCheckSuccess

	^true! !

!ResultAdoptionNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
assessmentResult

	^assessmentResult! !

!ResultAdoptionNotification methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
assessmentResult: anObject

	assessmentResult := anObject! !

!ResultAdoptionNotification methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	anEvaluationContext adoptResult: self assessmentResult.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'SqR 9/12/2015 11:27'!
buildLayoutMorph

	^LayoutMorph newColumn! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'sqr 5/25/2016 14:19'!
buildMorphicWindow

	| layout |
	layout := self buildLayoutMorph.
	self layoutMorph addMorph: layout! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'SqR 9/7/2015 16:57'!
defaultStripeHeight

	^40! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'sqr 5/25/2016 14:12'!
minimumExtent

	^400 @ 400! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'sqr 1/13/2016 21:56'!
separatorHeight

	^1! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'SqR 9/12/2015 11:37'!
windowColor

	| red green blue |
	red := (self windowColorBaseRGB at: 1) raisedTo: (self windowColorPowers at: 1).
	green := (self windowColorBaseRGB at: 2) raisedTo: (self windowColorPowers at: 2).
	blue := (self windowColorBaseRGB at: 3) raisedTo: (self windowColorPowers at: 3).
	^Color r: red g: green b: blue! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'SqR 9/12/2015 11:35'!
windowColorBaseRGB

	^#(0.2 0.9 0.2)! !

!AssessmentsWindow methodsFor: 'GUI building' stamp: 'SqR 9/12/2015 11:34'!
windowColorPowers

	^#(1.0 1.0 1.0)! !

!AssessmentsWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/7/2015 16:58'!
buildMorphicButtonAction: anAction label: aLabel

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: anAction
		label: aLabel! !

!AssessmentsWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/7/2015 16:59'!
buildMorphicSeparator

	^WidgetMorph new
		color: Color black;
		yourself! !

!AssessmentsWindow methodsFor: 'GUI building - widgets' stamp: 'jmv 9/6/2017 10:01:25'!
buildMorphicStatusBar

	| answer |
	answer := TextModelMorph
		textProvider: self model
		textGetter: #statusBarText.
	answer hideScrollBarsIndefinitely.
	answer disableEditing.
	^answer! !

!AssessmentsWindow methodsFor: 'actions' stamp: 'sqr 11/2/2019 19:43:22'!
cursorWaitWhile: aBlock

	Cursor waitCursor showWhile: [aBlock value]! !

!AssessmentsWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 16:59'!
refresh

	self model refresh! !

!AssessmentsWindow class methodsFor: 'subclass responsibility' stamp: 'SqR 9/7/2015 17:02'!
newEvaluator

	self subclassResponsibility! !

!AssessmentsWindow class methodsFor: 'subclass responsibility' stamp: 'SqR 9/7/2015 17:10'!
windowLabel

	self subclassResponsibility! !

!AssessmentsWindow class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:03'!
openEvaluator

	self openEvaluator: self newEvaluator! !

!AssessmentsWindow class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:03'!
openEvaluator: anEvaluator

	self open: anEvaluator label: self windowLabel! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 15:30'!
browse

	self model selectedChecklistDo:
		[:checklist |
			checklist classAndSelectorForBrowsingDo:
				[:class :selector | BrowserWindow fullOnClass: class selector: selector].
		]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 15:31'!
browseHierarchy

	self model selectedChecklistDo:
		[:checklist |
			checklist classAndSelectorForBrowsingDo:
				[:class :selector | HierarchyBrowserWindow onClass: class selector: selector].
		]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 14:33'!
evaluate

	self cursorWaitWhile: [self model evaluate]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 14:34'!
evaluateAll

	self cursorWaitWhile: [self model evaluateAll]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 14:34'!
evaluateHierarchy

	self cursorWaitWhile: [self model evaluateHierarchy]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/7/2015 17:03'!
spawn

	self model selectedChecklistDo: [:checklist | self class openEvaluatorRootedAt: checklist]! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building' stamp: 'sqr 5/25/2016 14:08'!
buildLayoutMorph

	^super buildLayoutMorph
		addMorph: self buildMorphicChecklistTree proportionalWidth: 1.0;
		addMorph: self buildMorphicSeparator fixedHeight: self separatorHeight;
		addMorph: self buildMorphicStatusBar fixedHeight: self defaultStripeHeight;
		addMorph: self buildMorphicSeparator fixedHeight: self separatorHeight;
		addMorph: self buildMorphicButtons fixedHeight: self defaultStripeHeight;
		yourself! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 01:33'!
buildMorphicButtons

	^LayoutMorph newRow
		addMorph: self buildMorphicEvaluateButton proportionalWidth: 0.25;
		addMorph: self buildMorphicEvaluateHierarchyButton proportionalWidth: 0.25;
		addMorph: self buildMorphicEvaluateAllButton proportionalWidth: 0.25;
		addMorph: self buildMorphicRefreshButton proportionalWidth: 0.25;
		yourself! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 02:38'!
buildMorphicChecklistTree

	| answer |
	answer := HierarchicalListMorph
		model: self model
		listGetter: #checklists
		indexGetter: #selectedChecklist
		indexSetter: #selectedChecklist:
		mainView: self
		menuGetter: #checklistMenu
		keystrokeAction: nil.
	answer autoDeselect: false.
	answer autoExpand: true.
	answer expandAll.
	^answer! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 02:07'!
buildMorphicEvaluateAllButton

	^self buildMorphicButtonAction: #evaluateAll label: 'Evaluate All'! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 02:07'!
buildMorphicEvaluateButton

	^self buildMorphicButtonAction: #evaluate label: 'Evaluate'! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 02:07'!
buildMorphicEvaluateHierarchyButton

	^self buildMorphicButtonAction: #evaluateHierarchy label: 'Evaluate Hierarchy'! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/6/2015 02:07'!
buildMorphicRefreshButton

	^self buildMorphicButtonAction: #refresh label: 'Refresh'! !

!AssessmentsChecklistEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'jmv 5/14/2018 12:01:59'!
checklistMenu

	^MenuMorph new
		defaultTarget: self;
		addTitle: 'Checklist';
		add: 'Browse...' action: #browse icon: #editFindReplaceIcon;
		add: 'Hierarchy...' action: #browseHierarchy icon: #editFindReplaceIcon;
		addLine;
		add: 'Spawn...' action: #spawn icon: #goTopIcon;
		yourself! !

!AssessmentsChecklistEvaluatorWindow class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:01'!
newEvaluator

	^AssessmentsChecklistEvaluator new! !

!AssessmentsChecklistEvaluatorWindow class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:03'!
openEvaluatorRootedAt: aChecklist

	| evaluator |
	evaluator := self newEvaluator.
	evaluator checkRepositoryRoot: aChecklist.
	self openEvaluator: evaluator! !

!AssessmentsChecklistEvaluatorWindow class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:02'!
windowLabel

	^'Checklist Evaluator'! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'sqr 1/25/2016 15:43'!
browse

	self model selectedResultDo:
		[:result |
			result classAndSelectorForBrowsingDo:
				[:class :selector | BrowserWindow fullOnClass: class selector: selector]
		]! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:44'!
copyAllToClipboard

	self privateCopyAsTextToClipboard: self model resultRoot! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:42'!
copyHierarchyToClipboard

	self model selectedResultDo:
		[:result | self privateCopyAsTextToClipboard: result]! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:26'!
debug

	self model selectedResultDo:
		[:result | [result debugResult] fork]! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/17/2015 03:18'!
reEvaluate

	self model reEvaluate.
	self reExpandResultTree! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:55'!
runToFailure

	self model selectedResultDo:
		[:result |
			[
				| newResult |
				newResult := result runResultToFailure.
				newResult isEmpty not
					:: and: [newResult allChecksPassed]
					:: ifTrue: [self inform: 'No error occurred']
			] fork
		]! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building' stamp: 'sqr 5/25/2016 14:09'!
buildLayoutMorph

	| newResultTree |
	newResultTree := self buildMorphicResultTree.
	self resultTree: newResultTree.
	^super buildLayoutMorph
		addMorph: newResultTree proportionalWidth: 1.0;
		addMorph: self buildMorphicSeparator fixedHeight: self separatorHeight;
		addMorph: self buildMorphicStatusBar fixedHeight: self defaultStripeHeight;
		addMorph: self buildMorphicSeparator fixedHeight: self separatorHeight;
		addMorph: self buildMorphicButtons fixedHeight: self defaultStripeHeight;
		yourself! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building' stamp: 'SqR 9/17/2015 03:11'!
buildMorphicWindow

	super buildMorphicWindow.
	self reExpandResultTree! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building' stamp: 'SqR 9/12/2015 11:37'!
windowColorPowers

	^#(0.2 1.3 0.5)! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/12/2015 11:41'!
buildMorphicButtons

	^LayoutMorph newRow
		addMorph: self buildMorphicDebugButton proportionalWidth: 0.33;
		addMorph: self buildMorphicRunToFailureButton proportionalWidth: 0.33;
		addMorph: self buildMorphicReEvaluateButton proportionalWidth: 0.33;
		yourself! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/12/2015 11:41'!
buildMorphicDebugButton

	^self buildMorphicButtonAction: #debug label: 'Debug'! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/12/2015 11:41'!
buildMorphicReEvaluateButton

	^self buildMorphicButtonAction: #reEvaluate label: 'Reevaluate Assessment'! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/17/2015 03:13'!
buildMorphicResultTree

	| answer |
	answer := HierarchicalListMorph
		model: self model
		listGetter: #results
		indexGetter: #selectedResult
		indexSetter: #selectedResult:
		mainView: self
		menuGetter: #resultMenu
		keystrokeAction: nil.
	answer autoDeselect: false.
	answer autoExpand: false.
	^answer! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'SqR 9/12/2015 11:41'!
buildMorphicRunToFailureButton

	^self buildMorphicButtonAction: #runToFailure label: 'Run to Failure'! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'GUI building - widgets' stamp: 'jmv 5/14/2018 12:00:51'!
resultMenu

	^MenuMorph new
		defaultTarget: self;
		addTitle: 'Result';
		add: 'Debug...' action: #debug icon: #debugIcon;
		add: 'Run to Failure...' action: #runToFailure icon: #mediaPlaybackStartIcon;
		addLine;
		add: 'Browse...' action: #browse icon: #editFindReplaceIcon;
		addLine;
		add: 'Copy hierarchy as text to clipboard...' action: #copyHierarchyToClipboard icon: #copyIcon;
		add: 'Copy all as text to clipboard...' action: #copyAllToClipboard icon: #copyIcon;
		yourself! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'private - actions' stamp: 'SqR 9/17/2015 02:41'!
privateCopyAsTextToClipboard: aCheckResult

	| textBuffer |
	textBuffer := String new writeStream.
	aCheckResult printClipboardTextOn: textBuffer.
	Clipboard storeObject: textBuffer contents! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'private - actions' stamp: 'SqR 9/17/2015 03:11'!
reExpandResultTree

	self resultTree expandAllAsPer:
		[:each | each complexContents shouldExpandAutomaticallyWhenViewed]! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'private - accessing' stamp: 'SqR 9/17/2015 03:08'!
resultTree

	^resultTree! !

!AssessmentsResultsEvaluatorWindow methodsFor: 'private - accessing' stamp: 'SqR 9/17/2015 03:09'!
resultTree: aMorph

	resultTree := aMorph! !

!AssessmentsResultsEvaluatorWindow class methodsFor: 'instance creation' stamp: 'SqR 9/12/2015 11:27'!
windowLabel

	^'Results Evaluator'! !

!AssessmentsListItemWrapper methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:56'!
newDisplayString

	^self item printString! !

!AssessmentsListItemWrapper methodsFor: 'converting' stamp: 'SqR 9/7/2015 16:49'!
asString

	^self displayString! !

!AssessmentsListItemWrapper methodsFor: 'private' stamp: 'SqR 9/7/2015 16:48'!
basicContents

	^Array new! !

!AssessmentsListItemWrapper methodsFor: 'testing' stamp: 'SqR 9/7/2015 16:48'!
canBeDragged

	^false! !

!AssessmentsListItemWrapper methodsFor: 'testing' stamp: 'SqR 9/7/2015 16:48'!
hasContents

	^self contents notEmpty! !

!AssessmentsListItemWrapper methodsFor: 'accessing' stamp: 'SqR 9/7/2015 16:49'!
contents

	contents notNil ifTrue: [^contents].
	self contents: self basicContents.
	^contents! !

!AssessmentsListItemWrapper methodsFor: 'accessing' stamp: 'SqR 9/12/2015 11:56'!
displayString

	displayString notNil ifTrue: [^displayString].
	self displayString: self newDisplayString.
	^displayString! !

!AssessmentsListItemWrapper methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 16:47'!
contents: aCollection

	contents := aCollection! !

!AssessmentsListItemWrapper methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 16:47'!
displayString: aString

	displayString := aString! !

!AssessmentAbstractResultUIListItem methodsFor: 'comparing' stamp: 'SqR 9/12/2015 11:54'!
<= aListItem

	^self displayString <= aListItem displayString! !

!AssessmentAbstractResultUIListItem methodsFor: 'private' stamp: 'SqR 9/12/2015 12:05'!
basicContents

	^OrderedCollection new! !

!AssessmentAbstractResultUIListItem methodsFor: 'private' stamp: 'SqR 9/7/2015 17:50'!
newRunToFailureChecklist

	^CustomRunToFailureChecklist new! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/12/2015 11:59'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self checkResult classForBrowsing value: nil! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:48'!
debugChecklist

	^self! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:48'!
debugResult

	self evaluateChecklist: self debugChecklist! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:48'!
evaluateChecklist: aChecklist

	^aChecklist assessment evaluationResult! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:49'!
newDebugChecklist

	^CustomDebugChecklist new! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:49'!
runResultToFailure

	^self evaluateChecklist: self runToFailureChecklist! !

!AssessmentAbstractResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:49'!
runToFailureChecklist

	^self! !

!AssessmentAbstractResultUIListItem methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	^self class name! !

!AssessmentAbstractResultUIListItem methodsFor: 'printing - clipboard' stamp: 'SqR 9/7/2015 17:50'!
printClipboardTextOn: aStream

	self
		printClipboardTextOn: aStream
		withIndentation:0! !

!AssessmentAbstractResultUIListItem methodsFor: 'printing - clipboard' stamp: 'SqR 9/7/2015 17:50'!
printClipboardTextOn: aStream
withIndentation: anInteger

	anInteger timesRepeat: [aStream tab].
	self printOn: aStream.
	aStream newLine! !

!AssessmentAbstractResultUIListItem class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:47'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!AssessmentResultUICheckResultListItem methodsFor: 'checklists' stamp: 'SqR 9/7/2015 17:52'!
collectAllChecksInto: aChecklist

	^self
		privateCollectAllChecksExcept: Set new
		into: aChecklist! !

!AssessmentResultUICheckResultListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:53'!
debugChecklist

	^self collectAllChecksInto: self newDebugChecklist! !

!AssessmentResultUICheckResultListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 17:53'!
runToFailureChecklist

	^self collectAllChecksInto: self newRunToFailureChecklist! !

!AssessmentResultUICheckResultListItem methodsFor: 'printing - clipboard' stamp: 'SqR 9/12/2015 12:04'!
printClipboardTextOn: aStream
withIndentation: anInteger

	super
		printClipboardTextOn: aStream
		withIndentation: anInteger.
	self contents do:
		[:each |
			each
				printClipboardTextOn: aStream
				withIndentation: anInteger + 1
		]! !

!AssessmentResultUICheckResultListItem methodsFor: 'private - checklists' stamp: 'SqR 9/12/2015 12:04'!
privateCollectAllChecksExcept: duplicates
into: aChecklist

	self contents do:
		[:each |
			each
				privateCollectAllChecksExcept: duplicates
				into: aChecklist
		].
	^aChecklist! !

!AssessmentResultUICheckResult methodsFor: 'accessing' stamp: 'SqR 9/7/2015 17:54'!
checkResult

	^checkResult! !

!AssessmentResultUICheckResult methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 17:56'!
checkResult: anObject

	checkResult := anObject! !

!AssessmentResultUICheckResult methodsFor: 'evaluation' stamp: 'sqr 1/13/2016 22:23'!
classAndSelectorForBrowsingDo: aBlock

	self checkResult classAndSelectorForBrowsingDo: aBlock! !

!AssessmentResultUICheckResult methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	| answer |
	answer := String new writeStream.
	self checkResult summaryPrintOn: answer.
	^answer contents! !

!AssessmentResultUICheckResult methodsFor: 'private - checklists' stamp: 'SqR 9/7/2015 17:56'!
privateCollectAllChecksExcept: duplicates
into: aChecklist

	self checkResult checkDo:
		[:check |
			(duplicates includes: check) ifFalse:
				[
					aChecklist addCheck: check.
					duplicates add: check
				]
		].
	^super
		privateCollectAllChecksExcept: duplicates
		into: aChecklist! !

!AssessmentResultUICheckResult class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:57'!
forCheckResult: aCheckResult

	^self new
		checkResult: aCheckResult;
		yourself! !

!AssessmentResultUICompositeResult methodsFor: 'private' stamp: 'SqR 9/12/2015 12:06'!
basicContents

	^SortedCollection new! !

!AssessmentResultUIResultFilter methodsFor: 'comparing' stamp: 'SqR 9/7/2015 18:00'!
<= aFilter

	self majorSortKey > aFilter majorSortKey ifTrue: [^false].
	self majorSortKey < aFilter majorSortKey ifTrue: [^true].
	^self minorSortKey <= aFilter minorSortKey! !

!AssessmentResultUIResultFilter methodsFor: 'private' stamp: 'SqR 9/12/2015 12:08'!
basicContents

	| answer |
	answer := Dictionary new.
	self results do:
		[:each |
			| classification composite |
			classification := each displayClassificationTag.
			composite := answer
				at: classification
				ifAbsentPut: [AssessmentResultUIResultPerClassification forClassification: classification].
			composite addChild: (self checkResultHolderFor: each)
		].
	^answer asSortedCollection! !

!AssessmentResultUIResultFilter methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:01'!
checkResultHolderFor: aCheckResult

	^AssessmentResultUICheckResult forCheckResult: aCheckResult! !

!AssessmentResultUIResultFilter methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:01'!
result: anObject

	result := anObject! !

!AssessmentResultUIResultFilter methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:00'!
classificationTag

	^self class! !

!AssessmentResultUIResultFilter methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:00'!
majorSortKey

	^String new! !

!AssessmentResultUIResultFilter methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:00'!
minorSortKey

	^String new! !

!AssessmentResultUIResultFilter methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	^self classificationTag name! !

!AssessmentResultUIResultFilter methodsFor: 'accessing' stamp: 'SqR 9/7/2015 17:59'!
result

	^result! !

!AssessmentResultUIResultFilter methodsFor: 'accessing' stamp: 'SqR 9/7/2015 18:00'!
results

	^self result checkResultsClassifiedAs: self classificationTag! !

!AssessmentResultUIResultFilter methodsFor: 'testing' stamp: 'SqR 9/7/2015 18:02'!
shouldExpandAutomaticallyWhenViewed

	^false! !

!AssessmentResultUIResultFilter class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 18:03'!
forResult: aResult

	^self new
		result: aResult;
		yourself! !

!AssessmentResultUIResultFilter class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 18:04'!
forResult: aResult andClassificationTag: aClassificationTag

	| genericAnswer |
	AssessmentResultUIResultFilterPreset subclasses do:
		[:one |
			| preset |
			preset := one forResult: aResult.
			preset classificationTag = aClassificationTag ifTrue: [^preset].
		].
	genericAnswer := AssessmentResultUIResultFilterOthers forResult: aResult.
	genericAnswer classificationTag: aClassificationTag.
	^genericAnswer! !

!AssessmentResultUIResultFilterOthers methodsFor: 'accessing' stamp: 'SqR 9/7/2015 18:05'!
classificationTag

	^classificationTag! !

!AssessmentResultUIResultFilterOthers methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:05'!
classificationTag: anObject

	classificationTag := anObject! !

!AssessmentResultUIResultFilterOthers methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:05'!
majorSortKey

	^self classificationTag preventsAssessmentSuccess
		ifTrue: ['2'] ifFalse: ['3']! !

!AssessmentResultUIResultFilterOthers methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:05'!
minorSortKey

	^self classificationTag name! !

!AssessmentResultUIResultFilterOthers methodsFor: 'testing' stamp: 'SqR 9/7/2015 18:05'!
shouldExpandAutomaticallyWhenViewed

	^self classificationTag preventsAssessmentSuccess! !

!AssessmentResultUIResultFilterPreset methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:06'!
majorSortKey

	^'1'! !

!AssessmentResultUIResultFilterErrors methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:06'!
classificationTag

	^self result errorTag! !

!AssessmentResultUIResultFilterErrors methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:07'!
minorSortKey

	^'1'! !

!AssessmentResultUIResultFilterErrors methodsFor: 'testing' stamp: 'SqR 9/7/2015 18:06'!
shouldExpandAutomaticallyWhenViewed

	^true! !

!AssessmentResultUIResultFilterFailures methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:07'!
classificationTag

	^self result failureTag! !

!AssessmentResultUIResultFilterFailures methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:07'!
minorSortKey

	^'2'! !

!AssessmentResultUIResultFilterFailures methodsFor: 'testing' stamp: 'SqR 9/7/2015 18:07'!
shouldExpandAutomaticallyWhenViewed

	^true! !

!AssessmentResultUIResultFilterPasses methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:08'!
classificationTag

	^self result benchmarkPassTag! !

!AssessmentResultUIResultFilterPasses methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:08'!
majorSortKey

	^'4'! !

!AssessmentResultUIResultFilterBenchmarkPasses methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:08'!
classificationTag

	^self result passTag! !

!AssessmentResultUIResultFilterBenchmarkPasses methodsFor: 'filtering' stamp: 'SqR 9/7/2015 18:09'!
majorSortKey

	^'5'! !

!AssessmentResultUIResultFilterRoot methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:04'!
adoptResult: aResult

	aResult tagsAndResultsDo:
		[:eachTag :someResults |
			| newChild |
			newChild := AssessmentResultUIResultFilter
				forResult: aResult
				andClassificationTag: eachTag.
			self contents add: newChild
		]! !

!AssessmentResultUIResultFilterRoot methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	^'Assessment Results'! !

!AssessmentResultUIResultFilterRoot methodsFor: 'testing' stamp: 'SqR 9/12/2015 12:19'!
shouldExpandAutomaticallyWhenViewed

	^true! !

!AssessmentResultUIResultFilterRoot class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 18:09'!
forResult: aResult

	^self new
		adoptResult: aResult;
		yourself! !

!AssessmentResultUIResultPerClassification methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:04'!
addChild: anObject

	self contents add: anObject! !

!AssessmentResultUIResultPerClassification methodsFor: 'evaluation' stamp: 'SqR 9/12/2015 12:00'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self classification value: nil! !

!AssessmentResultUIResultPerClassification methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:12'!
classification

	^classification! !

!AssessmentResultUIResultPerClassification methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:12'!
classification: anObject

	classification := anObject! !

!AssessmentResultUIResultPerClassification methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	^self classification printString! !

!AssessmentResultUIResultPerClassification methodsFor: 'testing' stamp: 'SqR 9/12/2015 12:24'!
shouldExpandAutomaticallyWhenViewed

	^true! !

!AssessmentResultUIResultPerClassification class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 18:11'!
forClassification: aClassification

	^self new
		classification: aClassification;
		yourself! !

!CheckBenchmarkResultUIListItem methodsFor: 'display' stamp: 'SqR 9/7/2015 18:14'!
checklist

	^self result check checklist printString! !

!CheckBenchmarkResultUIListItem methodsFor: 'display' stamp: 'SqR 9/7/2015 18:14'!
iterationsPerSecond

	^self result iterationsPerSecondPrintString! !

!CheckBenchmarkResultUIListItem methodsFor: 'display' stamp: 'SqR 9/7/2015 18:15'!
measurement

	^self result prettyPrint! !

!CheckBenchmarkResultUIListItem methodsFor: 'display' stamp: 'SqR 9/7/2015 18:15'!
scenario

	^self result prettyPrintForScenario! !

!CheckBenchmarkResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/12/2015 12:00'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self result classForBrowsing value: nil! !

!CheckBenchmarkResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 18:15'!
debugChecklist

	^self newDebugChecklist
		addCheck: self result check;
		yourself! !

!CheckBenchmarkResultUIListItem methodsFor: 'evaluation' stamp: 'SqR 9/7/2015 18:15'!
runToFailureChecklist

	^self newRunToFailureChecklist
		addCheck: self result check;
		yourself! !

!CheckBenchmarkResultUIListItem methodsFor: 'printing' stamp: 'SqR 9/12/2015 11:57'!
newDisplayString

	| answer |
	answer := String new writeStream.
	self result summaryPrintOn: answer.
	^answer contents! !

!CheckBenchmarkResultUIListItem methodsFor: 'accessing' stamp: 'SqR 9/7/2015 18:13'!
result

	^result! !

!CheckBenchmarkResultUIListItem methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 18:15'!
result: anObject

	result := anObject! !

!CheckBenchmarkResultUIListItem class methodsFor: 'instance creation' stamp: 'SqR 9/12/2015 11:50'!
forResult: aResult

	^self new
		result: aResult;
		yourself! !

!CheckBenchmarkExpectationUIListItem methodsFor: 'display' stamp: 'SqR 9/7/2015 18:16'!
expectedIterationsPerSecond

	^self result shortActualSpeedPrintString! !

!CheckBenchmarkExpectationUIListItem methodsFor: 'display' stamp: 'sqr 1/14/2016 01:33'!
passed

	^self result preventsAssessmentSuccess not! !

!AssessmentsChecklistWrapper methodsFor: 'private' stamp: 'SqR 9/5/2015 16:07'!
basicContents

	| checklistSubclasses visibleSubclasses sortedSubclasses |
	checklistSubclasses := self checklist checkSelectorLookupSubclasses.
	visibleSubclasses := checklistSubclasses select: [:any | any shouldBeDisplayedInChecklistEvaluator].
	sortedSubclasses := visibleSubclasses asSortedCollection: [:x :y | x name < y name].
	^sortedSubclasses collect: [:each | self class with: each]! !

!AssessmentsChecklistWrapper methodsFor: 'accessing' stamp: 'SqR 9/5/2015 15:47'!
checklist

	^self item! !

!AssessmentsChecklistWrapper methodsFor: 'private - accessing' stamp: 'SqR 9/12/2015 11:51'!
checklist: aChecklist

	self setItem: aChecklist! !

!AssessmentsEvaluator methodsFor: 'private - status bar text' stamp: 'SqR 9/7/2015 17:15'!
printStatusFor: aCollection labeled: aString on: aStream

	aCollection isEmpty ifTrue: [^self].
	aStream
		nextPutAll: ', ';
		print: aCollection size;
		nextPutAll: aString! !

!AssessmentsEvaluator methodsFor: 'private - status bar text' stamp: 'SqR 9/7/2015 17:15'!
statusBarTextFor: aResult

	| answer |
	answer := String new writeStream.
	answer print: aResult checkExecutionCount; nextPutAll: ' results'.
	self printStatusFor: aResult skips labeled: ' skipped' on: answer.
	self printStatusFor: aResult errors labeled: ' errors' on: answer.
	self printStatusFor: aResult failures labeled: ' failures' on: answer.
	self printStatusFor: aResult validationFailures labeled: ' validation failures' on: answer.
	self printStatusFor: aResult benchmarkExpectationsFailed labeled: ' benchmark expectations failed' on: answer.
	^answer nextPut: $.; contents! !

!AssessmentsEvaluator methodsFor: 'testing' stamp: 'SqR 9/7/2015 17:14'!
shouldProfileEvaluation

	^AssessmentsProfiler current shouldProfileEvaluation! !

!AssessmentsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/7/2015 17:13'!
statusBarText

	^statusBarText ifNil: ['...']! !

!AssessmentsEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 17:14'!
statusBarText: aString

	statusBarText := aString.
	self changed: #acceptedContents! !

!AssessmentsEvaluator methodsFor: 'private - checklists' stamp: 'SqR 9/7/2015 17:15'!
visibleSortedSubclassesOf: aChecklist

	| visibleSubclasses |
	visibleSubclasses := self visibleSubclassesOf: aChecklist.
	^visibleSubclasses asSortedCollection: [:x :y | x name < y name]! !

!AssessmentsEvaluator methodsFor: 'private - checklists' stamp: 'SqR 9/7/2015 17:16'!
visibleSubclassesOf: aChecklist

	^aChecklist checkSelectorLookupSubclasses
		select: [:any | any shouldBeDisplayedInChecklistEvaluator]! !

!AssessmentsEvaluator class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:29'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!AssessmentsChecklistEvaluator methodsFor: 'private - actions' stamp: 'SqR 9/6/2015 02:42'!
assessmentForChecklists: aCollection

	| assessment |
	assessment := aCollection first assessment.
	2 to: aCollection size do: [:each | assessment addChecklist: (aCollection at: each)].
	^assessment! !

!AssessmentsChecklistEvaluator methodsFor: 'private - actions' stamp: 'SqR 9/6/2015 02:48'!
evaluate: anAssessment

	| evaluationContext |
	self shouldProfileEvaluation ifFalse: [^anAssessment evaluationResult].
	evaluationContext := anAssessment defaultEvaluationContext.
	evaluationContext executionPolicy: TimeProfilerAssessmentExecutionPolicy new.
	^evaluationContext evaluate: anAssessment! !

!AssessmentsChecklistEvaluator methodsFor: 'private - actions' stamp: 'SqR 9/6/2015 02:42'!
evaluateAssessmentForAllVisibleSubclassesOf: aChecklist

	| checklistsToEvaluate pivotIndex |
	pivotIndex := 1.
	checklistsToEvaluate := OrderedCollection with: aChecklist.
	[pivotIndex > checklistsToEvaluate size] whileFalse:
		[
			| thisChecklist |
			thisChecklist := checklistsToEvaluate at: pivotIndex.
			checklistsToEvaluate addAll: (self visibleSubclassesOf: thisChecklist).
			pivotIndex := pivotIndex + 1
		].
	self evaluateAssessmentForChecklists: checklistsToEvaluate! !

!AssessmentsChecklistEvaluator methodsFor: 'private - actions' stamp: 'SqR 9/7/2015 17:25'!
evaluateAssessmentForChecklists: aCollection

	| assessment result newStatusBarText |
	self statusBarText: 'Running...'.
	assessment := self assessmentForChecklists: aCollection.
	result := self evaluate: assessment.
	newStatusBarText := self statusBarTextFor: result.
	self statusBarText: newStatusBarText.
	self openAssessment: assessment withResult: result! !

!AssessmentsChecklistEvaluator methodsFor: 'private - actions' stamp: 'SqR 9/7/2015 17:39'!
openAssessment: assessment withResult: result

	| evaluator |
	evaluator := AssessmentsResultsEvaluator assessment: assessment result: result.
	AssessmentsResultsEvaluatorWindow openEvaluator: evaluator! !

!AssessmentsChecklistEvaluator methodsFor: 'private' stamp: 'SqR 9/5/2015 16:09'!
basicChecklists

	| root |
	root := AssessmentsChecklistWrapper with: self checkRepositoryRoot.
	^Array with: root! !

!AssessmentsChecklistEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 14:55'!
checkRepositoryRoot

	^checkRepositoryRoot ifNil: [CheckRepository]! !

!AssessmentsChecklistEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 14:56'!
checkRepositoryRoot: aCheckRepository

	checkRepositoryRoot := aCheckRepository! !

!AssessmentsChecklistEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/5/2015 16:59'!
checklists: aCollection

	checklists := aCollection.
	self changed: #checklists! !

!AssessmentsChecklistEvaluator methodsFor: 'accessing' stamp: 'SqR 9/7/2015 15:02'!
checklists

	checklists notNil ifTrue: [^checklists].
	self refreshChecklists.
	^checklists! !

!AssessmentsChecklistEvaluator methodsFor: 'accessing' stamp: 'SqR 9/6/2015 02:38'!
selectedChecklist

	^selectedChecklist! !

!AssessmentsChecklistEvaluator methodsFor: 'accessing' stamp: 'SqR 9/6/2015 02:39'!
selectedChecklist: aSelection

	selectedChecklist := aSelection.
	self changed: #selectedChecklist! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/6/2015 02:40'!
evaluate

	self selectedChecklistDo: [:checklist | self evaluateAssessmentForChecklists: (Array with: checklist)]! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/6/2015 02:40'!
evaluateAll

	self refreshChecklists.
	self evaluateAssessmentForAllVisibleSubclassesOf: self checkRepositoryRoot! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/6/2015 02:40'!
evaluateHierarchy

	self refreshChecklists.
	self selectedChecklistDo:
		[:checklist | self evaluateAssessmentForAllVisibleSubclassesOf: checklist]! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/6/2015 02:50'!
refresh

	self refreshChecklists! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/6/2015 02:40'!
refreshChecklists

	self checklists: self basicChecklists! !

!AssessmentsChecklistEvaluator methodsFor: 'actions' stamp: 'SqR 9/12/2015 12:02'!
selectedChecklistDo: aBlock

	| selection |
	selection := self selectedChecklist.
	selection isNil ifTrue: [^self].
	^aBlock value: selection item! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/7/2015 17:27'!
assessment

	^assessment! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/7/2015 17:28'!
result

	^result! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/17/2015 02:44'!
resultRoot

	^self results first! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/17/2015 03:26'!
results

	results notNil ifTrue: [^results].
	self refreshResults.
	^results! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:01'!
selectedResult

	^selectedResult! !

!AssessmentsResultsEvaluator methodsFor: 'accessing' stamp: 'SqR 9/12/2015 12:02'!
selectedResult: aSelection

	selectedResult := aSelection.
	self changed: #selectedResult! !

!AssessmentsResultsEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 17:28'!
assessment: anAssessment

	assessment := anAssessment! !

!AssessmentsResultsEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/7/2015 17:28'!
result: aResult

	result := aResult! !

!AssessmentsResultsEvaluator methodsFor: 'private - accessing' stamp: 'SqR 9/12/2015 11:47'!
results: aCollection

	results := aCollection.
	self changed: #results! !

!AssessmentsResultsEvaluator methodsFor: 'private' stamp: 'SqR 9/12/2015 11:49'!
basicResults

	| root |
	root := AssessmentResultUIResultFilterRoot forResult: self result.
	^Array with: root! !

!AssessmentsResultsEvaluator methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:20'!
reEvaluate

	self result: self assessment evaluationResult.
	self refreshResults! !

!AssessmentsResultsEvaluator methodsFor: 'actions' stamp: 'SqR 9/17/2015 02:21'!
refreshResults

	self selectedResult: nil.
	self results: self basicResults! !

!AssessmentsResultsEvaluator methodsFor: 'actions' stamp: 'SqR 9/16/2015 17:05'!
selectedResultDo: aBlock

	| selection |
	selection := self selectedResult.
	selection isNil ifTrue: [^self].
	^aBlock value: selection! !

!AssessmentsResultsEvaluator class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:29'!
assessment: anAssessment result: aResult

	^self new
		assessment: anAssessment;
		result: aResult;
		yourself! !

!AbstractCustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorLookupRoot

	^self! !

!AbstractCustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^Array new! !

!AbstractCustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
usesClassBasedPrerequisites
	"Custom checklists collect prerequisites by asking checks"

	^true! !

!AbstractCustomChecklist methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
assessment

	^self assessmentClass forChecklist: self! !

!AbstractCustomChecklist methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
assessmentClass

	^Assessment! !

!AbstractCustomChecklist methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
installContextForCheck: aCheck

	^self! !

!AbstractCustomChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	| answer |
	answer := 0.
	self checksDo: [:each | answer := answer + each checkExecutionCount].
	^answer! !

!AbstractCustomChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	^self! !

!AbstractCustomChecklist methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	^self! !

!AbstractCustomChecklist methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
isAbstract

	^false! !

!AbstractCustomChecklist methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks

	^false! !

!AbstractCustomChecklist class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:17'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!CustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addCheck: aCheck

	self checks add: aCheck! !

!CustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecklist: aChecklist

	aChecklist checksDo: [:each | self addCheck: each]! !

!CustomChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self checks
		inject: Set new
		into: [:answer :each | answer addAll: each prerequisites; yourself]! !

!CustomChecklist methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checks

	^checks! !

!CustomChecklist methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checks: anObject

	checks := anObject! !

!CustomChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	^self checks do: aBlock! !

!CustomChecklist methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self checks: OrderedCollection new! !

!CustomDebugChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	^self checks do: [:each | aBlock value: each asDebugCheck]! !

!CustomRunToFailureChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	^self checks do: [:each | aBlock value: each asRunToFailureCheck]! !

!CustomSingleThreadRunToFailureChecklist methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	^self checks do: [:each | aBlock value: each asSingleThreadRunToFailureCheck]! !

!AbstractEvaluationContextPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext

	^evaluationContext! !

!AbstractEvaluationContextPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext: anObject

	evaluationContext := anObject! !

!AbstractEvaluationContextPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	^self! !

!AbstractEvaluationContextPolicy class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:17'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!AbstractAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateAssessment

	^self! !

!AbstractAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingAssessment

	^self! !

!AbstractAssessmentResultPolicy methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
addResult: aCheckResult

	^self! !

!AbstractAssessmentResultPolicy methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
adoptResult: anAssessmentResult

	anAssessmentResult resultsDo: [:each | self addResult: each]! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateAssessment

	^self! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - results' stamp: 'SqR 9/17/2015 03:02'!
appendFileStream

	^AssessmentsFileDirectory current appendFileStreamFor: self fileName! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
log: aString on: aWriteStream

	aWriteStream
		nextPutAll: self timestampLogString;
		nextPutAll: ' - ';
		nextPutAll: aString! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
print: aCheckResult on: aWriteStream

	self log: aCheckResult printString on: aWriteStream! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - results' stamp: 'SqR 9/17/2015 03:03'!
writeFileStream

	^AssessmentsFileDirectory current writeFileStreamFor: self fileName! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
fileName

	^fileName! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
fileName: anObject

	fileName := anObject! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
numberOfChecks

	^self evaluationContext assessment checks size! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - time' stamp: 'sqr 1/12/2016 19:39'!
timestampLogString

	^self useUTCTimestamps
		ifTrue: [AssessmentsDateAndTime current nowUTCPrintString]
		ifFalse: [AssessmentsDateAndTime current nowLocalPrintString]! !

!AbstractFileLogAssessmentResultPolicy methodsFor: 'private - time' stamp: 'SqR 3/8/2015 18:10'!
useUTCTimestamps

	^true! !

!AbstractFileLogAssessmentResultPolicy class methodsFor: 'instance creation'!
forFileNamed: aString

	^self new
		fileName: aString;
		yourself! !

!HardenedFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'sqr 10/4/2018 19:48:32'!
aboutToEvaluateAssessment

	| appendStream |
	super aboutToEvaluateAssessment.
	appendStream := self appendFileStream.
	[
		self
			log: 'Assessment created with ', self numberOfChecks printString, ' checks.'
			on: appendStream.
		appendStream newLine
	] ensure: [appendStream close]! !

!HardenedFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'sqr 10/4/2018 19:48:42'!
doneEvaluatingAssessment

	| appendStream |
	super doneEvaluatingAssessment.
	appendStream := self appendFileStream.
	[
		self
			log: 'Done evaluating assessment, ', self numberOfChecks printString, ' checks.'
			on: appendStream.
		appendStream newLine
	] ensure: [appendStream close]! !

!HardenedFileLogAssessmentResultPolicy methodsFor: 'results' stamp: 'sqr 10/4/2018 19:48:37'!
addResult: aCheckResult

	| appendStream |
	super addResult: aCheckResult.
	appendStream := self appendFileStream.
	[
		self print: aCheckResult on: appendStream.
		appendStream newLine
	] ensure: [appendStream close]! !

!SoftFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'sqr 10/4/2018 19:48:50'!
aboutToEvaluateAssessment

	super aboutToEvaluateAssessment.
	self
		log: 'Assessment created with ', self numberOfChecks printString, ' checks.'
		on: self writeStream.
	self writeStream newLine! !

!SoftFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'sqr 10/4/2018 19:48:59'!
doneEvaluatingAssessment

	super doneEvaluatingAssessment.
	self
		log: 'Done evaluating assessment, ', self numberOfChecks printString, ' checks.'
		on: self writeStream.
	self writeStream newLine! !

!SoftFileLogAssessmentResultPolicy methodsFor: 'results' stamp: 'sqr 10/4/2018 19:48:55'!
addResult: aCheckResult

	super addResult: aCheckResult.
	self print: aCheckResult on: self writeStream.
	self writeStream newLine! !

!SoftFileLogAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
writeStream

	^writeStream! !

!SoftFileLogAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
writeStream: anObject

	writeStream := anObject! !

!BufferedFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateAssessment

	self writeStream: String new writeStream.
	super aboutToEvaluateAssessment! !

!BufferedFileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingAssessment

	| fileStream |
	super doneEvaluatingAssessment.
	fileStream := self writeFileStream.
	[fileStream nextPutAll: self writeStream contents]
		ensure: [fileStream close]! !

!FileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateAssessment

	self writeStream: self writeFileStream.
	super aboutToEvaluateAssessment! !

!FileLogAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingAssessment

	super doneEvaluatingAssessment.
	self writeStream close! !

!DefaultAssessmentResultPolicy methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
addResult: aCheckResult

	super addResult: aCheckResult.
	self result addResult: aCheckResult! !

!DefaultAssessmentResultPolicy methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
result

	^result! !

!DefaultAssessmentResultPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self result: AssessmentResult new! !

!DefaultAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
result: anObject

	result := anObject! !

!CompositeAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateAssessment

	super aboutToEvaluateAssessment.
	self policiesDo: [:each | each aboutToEvaluateAssessment]! !

!CompositeAssessmentResultPolicy methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingAssessment

	super doneEvaluatingAssessment.
	self policiesDo: [:each | each doneEvaluatingAssessment]! !

!CompositeAssessmentResultPolicy methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addPolicy: aResultPolicy

	self policies add: aResultPolicy! !

!CompositeAssessmentResultPolicy methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
policies

	^policies! !

!CompositeAssessmentResultPolicy methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
addResult: aCheckResult

	super addResult: aCheckResult.
	self policiesDo: [:each | each addResult: aCheckResult]! !

!CompositeAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext: anEvaluationContext

	super evaluationContext: anEvaluationContext.
	self policiesDo: [:each | each evaluationContext: anEvaluationContext]! !

!CompositeAssessmentResultPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
policies: anObject

	policies := anObject! !

!CompositeAssessmentResultPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self policies: OrderedCollection new! !

!CompositeAssessmentResultPolicy methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
policiesDo: aBlock

	self policies do: aBlock! !

!DefaultAssessmentEvaluationPolicy methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateCheck: aCheck

	aCheck evaluateInTheContextOf: self evaluationContext! !

!DefaultAssessmentEvaluationPolicy methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateContext

	self evaluationContext checksDo: [:each | self evaluateCheck: each]! !

!FastPassAssessmentEvaluationPolicy methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateCheck: aCheck

	aCheck fastPassEvaluateInTheContextOf: self evaluationContext! !

!DefaultAssessmentExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
executeEvaluationOf: anEvaluationContext

	anEvaluationContext evaluateAssessment! !

!TimeProfilerAssessmentExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
executeEvaluationOf: anAssessment

	^self profile: [super executeEvaluationOf: anAssessment]! !

!TimeProfilerAssessmentExecutionPolicy methodsFor: 'private - execution' stamp: 'SqR 9/7/2015 14:46'!
profile: aBlock

	AssessmentsProfiler current profile: aBlock! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStartPrerequisites: aCollection

	aCollection do:
		[:each |
			self
				ifAnAttemptToStartPrerequisite: each
				succeeds: [self startedPrerequisites add: each]
				otherwise: [^false]
		].
	^true! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStopPrerequisites: aCollection

	aCollection reverseDo:
		[:each |
			| attempt |
			attempt := AttemptToStopPrerequisite prerequisite: each.
			attempt evaluateInTheContextOf: self evaluationContext
		]! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
ensurePrerequisitesFor: aCheck
	"This prerequisite policy requires all prerequisites to be started before
	execution begins.  Thus, do nothing"

	^true! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
potentiallyResortChecks: aCollection
	"This prerequisite policy requires all prerequisites to be started before
	execution begins.  Thus, it is not necessary to resort checks in advance"

	^aCollection! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesMet

	^self attemptToStartPrerequisites: self prerequisites! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesNoLongerNeeded

	self attemptToStopPrerequisites: self startedPrerequisites.
	self startedPrerequisites removeAll! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
ifAnAttemptToStartPrerequisite: aPrerequisite
succeeds: successBlock
otherwise: failureBlock

	| attempt answer |
	attempt := AttemptToStartPrerequisite prerequisite: aPrerequisite.
	answer := attempt evaluateInTheContextOf: self evaluationContext.
	answer ifTrue: [successBlock value] ifFalse: [failureBlock value]! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self startedPrerequisites: OrderedCollection new! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self evaluationContext prerequisites! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
startedPrerequisites: anObject

	startedPrerequisites := anObject! !

!DefaultAssessmentPrerequisitePolicy methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
startedPrerequisites

	^startedPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
changedCurrentPrerequisitesTo: currentPrerequisites

	self attemptToStopPrerequisites: self startedPrerequisites.
	self startedPrerequisites removeAll.
	^self attemptToStartPrerequisites: currentPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private - prerequisites' stamp: 'sqr 1/13/2016 21:53'!
checksSortedByPrerequisites: aCollection
	"Use sorted collections for the dialects in which sets are compared by identity"

	| checksByPrerequisites |
	checksByPrerequisites := Dictionary new.
	aCollection do:
		[:eachCheck |
			| checks |
			checks := checksByPrerequisites
				at: eachCheck prerequisites asSet asSortedCollection
				ifAbsentPut: [OrderedCollection new].
			checks add: eachCheck.
		].
	^checksByPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
startedPrerequisitesMatch: currentPrerequisitesSet

	^self startedPrerequisites size = currentPrerequisitesSet size and:
		[self startedPrerequisites allSatisfy: [:each | currentPrerequisitesSet includes: each]]! !

!LazyPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
ensurePrerequisitesFor: aCheck

	| currentPrerequisites currentPrerequisitesSet |
	currentPrerequisites := aCheck prerequisites.
	currentPrerequisitesSet := currentPrerequisites asSet.
	^self lastCheckPrerequisites = currentPrerequisitesSet
		ifTrue: [self startedPrerequisitesMatch: currentPrerequisitesSet]
		ifFalse:
			[
				self lastCheckPrerequisites: currentPrerequisitesSet.
				self changedCurrentPrerequisitesTo: currentPrerequisites
			]! !

!LazyPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
potentiallyResortChecks: aCollection

	| answer checksByPrerequisites |
	answer := OrderedCollection new.
	checksByPrerequisites := self checksSortedByPrerequisites: aCollection.
	checksByPrerequisites do: [:eachGroup | answer addAll: eachGroup].
	^answer! !

!LazyPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesMet

	^true! !

!LazyPrerequisitePolicy methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesNoLongerNeeded

	super prerequisitesNoLongerNeeded.
	self initializeLastCheckPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self initializeLastCheckPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initializeLastCheckPrerequisites

	self lastCheckPrerequisites: Set new! !

!LazyPrerequisitePolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
lastCheckPrerequisites

	^lastCheckPrerequisites! !

!LazyPrerequisitePolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
lastCheckPrerequisites: anObject

	lastCheckPrerequisites := anObject! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
addResult: aCheckResult

	self resultPolicy addResult: aCheckResult! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
adoptResult: anAssessmentResult

	self resultPolicy adoptResult: anAssessmentResult! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateAssessment

	self resultPolicy aboutToEvaluateAssessment.
	self evaluationPolicy evaluateContext.
	self resultPolicy doneEvaluatingAssessment! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
ensurePrerequisitesFor: aCheck

	^self prerequisitePolicy ensurePrerequisitesFor: aCheck! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateAssessment

	[self prerequisitesMet ifTrue: [self basicEvaluateAssessment]]
		ensure: [self prerequisitesNoLongerNeeded]! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesMet

	^self prerequisitePolicy prerequisitesMet! !

!AssessmentEvaluationContext methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesNoLongerNeeded

	^self prerequisitePolicy prerequisitesNoLongerNeeded! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
assessment

	^assessment! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
evaluationPolicy

	^evaluationPolicy! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
evaluationPolicy: anObject

	evaluationPolicy := anObject.
	anObject evaluationContext: self! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
executionPolicy

	^executionPolicy! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
executionPolicy: anObject

	executionPolicy := anObject.
	anObject evaluationContext: self! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisitePolicy

	^prerequisitePolicy! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisitePolicy: anObject

	prerequisitePolicy := anObject.
	anObject evaluationContext: self! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
result

	^self resultPolicy result! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
resultPolicy

	^resultPolicy! !

!AssessmentEvaluationContext methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
resultPolicy: anObject

	resultPolicy := anObject.
	anObject evaluationContext: self! !

!AssessmentEvaluationContext methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
assessment: anObject

	assessment := anObject! !

!AssessmentEvaluationContext methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
executionParameters

	^executionParameters! !

!AssessmentEvaluationContext methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
executionParameters: anObject

	executionParameters := anObject! !

!AssessmentEvaluationContext methodsFor: 'private - delegation' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	| sortedChecks |
	sortedChecks := self prerequisitePolicy potentiallyResortChecks: self assessment checks.
	sortedChecks do: aBlock! !

!AssessmentEvaluationContext methodsFor: 'private - delegation' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self assessment prerequisites! !

!AssessmentEvaluationContext methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluate: anAssessment

	self assessment: anAssessment.
	self executionPolicy executeEvaluationOf: self.
	^self result! !

!AssessmentEvaluationContext methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
validate: anObject with: anAssessment

	self objectToValidate: anObject.
	^self evaluate: anAssessment! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
evaluationPolicyClass

	^FastPassAssessmentEvaluationPolicy! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
executionParametersClass

	^AssessmentExecutionParameters! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
executionPolicyClass

	^DefaultAssessmentExecutionPolicy! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self evaluationPolicy: self evaluationPolicyClass new.
	self executionPolicy: self executionPolicyClass new.
	self prerequisitePolicy: self prerequisitePolicyClass new.
	self resultPolicy: self resultPolicyClass new.
	self executionParameters: self executionParametersClass new! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
prerequisitePolicyClass

	^LazyPrerequisitePolicy! !

!AssessmentEvaluationContext methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
resultPolicyClass

	^DefaultAssessmentResultPolicy! !

!AssessmentEvaluationContext methodsFor: 'accessing - execution parameters' stamp: 'SqR 3/8/2015 18:10'!
objectToValidate

	^self executionParameters objectToValidate! !

!AssessmentEvaluationContext methodsFor: 'accessing - execution parameters' stamp: 'SqR 3/8/2015 18:10'!
objectToValidate: anObject

	self executionParameters objectToValidate: anObject! !

!AssessmentEvaluationContext methodsFor: 'accessing - execution parameters' stamp: 'SqR 3/8/2015 18:10'!
originalObjectToValidate

	^self executionParameters originalObjectToValidate! !

!AssessmentEvaluationContext methodsFor: 'accessing - execution parameters' stamp: 'SqR 3/8/2015 18:10'!
originalObjectToValidate: anObject

	self executionParameters originalObjectToValidate: anObject! !

!AssessmentEvaluationContext class methodsFor: 'instance creation'!
evaluate: anAssessment

	^self new evaluate: anAssessment! !

!AssessmentEvaluationContext class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:18'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!AssessmentExecutionParameters methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
objectToValidate

	^objectToValidate! !

!AssessmentExecutionParameters methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
originalObjectToValidate

	^originalObjectToValidate! !

!AssessmentExecutionParameters methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
objectToValidate: anObject

	objectToValidate := anObject! !

!AssessmentExecutionParameters methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
originalObjectToValidate: anObject

	originalObjectToValidate := anObject! !

!AssessmentExecutionParameters class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:34'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!AbstractPrerequisite methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint

	^'A prerequisite'! !

!AbstractPrerequisite methodsFor: 'comparing' stamp: 'sqr 1/13/2016 21:51'!
<= aPrerequisite
	"For the dialects in which Set new = Set new evaluates to false"

	^self class name <= aPrerequisite class name! !

!AbstractPrerequisite methodsFor: 'comparing' stamp: 'SqR 3/8/2015 18:10'!
= aPrerequisite
	"Assume these are singletons, refine as needed"

	^self class = aPrerequisite class! !

!AbstractPrerequisite methodsFor: 'comparing' stamp: 'sqr 1/14/2016 01:10'!
hash

	^self class hash! !

!AbstractPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStart

	^true! !

!AbstractPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStop

	^true! !

!AbstractPrerequisite methodsFor: 'private - actions' stamp: 'SqR 2/22/2015 20:04'!
failToStart

	self failureNotification signal: self prettyPrint, ' has failed to start'! !

!AbstractPrerequisite methodsFor: 'private - actions' stamp: 'SqR 2/22/2015 20:04'!
failToStop

	self failureNotification signal: self prettyPrint, ' has failed to stop'! !

!AbstractPrerequisite methodsFor: 'private - actions' stamp: 'SqR 2/22/2015 20:04'!
refuseToStart

	self skipNotification signal: self prettyPrint, ' has refused to start'! !

!AbstractPrerequisite methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:21'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self receiver classForBrowsing value: nil! !

!AbstractPrerequisite methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
failureNotification

	^PrerequisiteFailureNotification! !

!AbstractPrerequisite methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
skipNotification

	^CheckSkipNotification! !

!AbstractPrerequisite methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
start
	"If the prerequisite cannot be met, then fail with an error"

	self attemptToStart ifFalse: [self failToStart]! !

!AbstractPrerequisite methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
stop
	"If the prerequisite cannot stop, then fail with an error"

	self attemptToStop ifFalse: [self failToStop]! !

!AbstractPrerequisite class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:32'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!ChecklistPrerequisite methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint

	^self class name! !

!NonStartableChecklistPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStart

	^false! !

!NonStoppableChecklistPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStop

	^false! !

!NotifyingChecklistPrerequisite methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
start

	super start.
	self triggerEvent: #prerequisiteStarted! !

!NotifyingChecklistPrerequisite methodsFor: 'actions' stamp: 'SqR 3/8/2015 18:10'!
stop

	self removeActionsForEvent: #prerequisiteStarted.
	^super stop! !

!StartErrorChecklistPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStart

	1 / 0! !

!StopErrorChecklistPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStop

	1 / 0! !

!SUnitPrerequisite methodsFor: 'comparing' stamp: 'SqR 3/8/2015 18:10'!
= aPrerequisite

	^super = aPrerequisite
		and: [self testResource = aPrerequisite testResource]! !

!SUnitPrerequisite methodsFor: 'comparing' stamp: 'SqR 3/8/2015 18:10'!
hash

	^super hash bitXor: self testResource hash! !

!SUnitPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStart
	"If a resource is not available, it may be because a
	prior start attempt failed.  Rather than causing manual
	intervention, try resetting the resource once"

	^self testResource isAvailable or:
		[
			self testResource reset.
			self testResource isAvailable
		]! !

!SUnitPrerequisite methodsFor: 'private - actions' stamp: 'SqR 3/8/2015 18:10'!
attemptToStop
	"Do not answer

		^self testResource isUnavailable

	because SUnitToo will restart the resource
	trying to answer isUnavailable"

	self testResource reset.
	^true! !

!SUnitPrerequisite methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:21'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self testResource classForBrowsing value: nil! !

!SUnitPrerequisite methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
testResource

	^testResource! !

!SUnitPrerequisite methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
testResource: anObject

	testResource := anObject! !

!SUnitPrerequisite class methodsFor: 'instance creation'!
for: aTestResource

	^self new
		testResource: aTestResource;
		yourself! !

!Assessment methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecklist: aChecklistClass

	aChecklistClass isAbstract ifTrue: [^self].
	self addChecklist: aChecklistClass inTheContextOf: aChecklistClass! !

!Assessment methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecks: aCollection

	self privateAddChecks: aCollection.
	aCollection do: [:each | self prerequisites addAll: each prerequisites]! !

!Assessment methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecksWithSelectors: checkSelectorInclusionSet
startingAtChecklist: aChecklistClass

	aChecklistClass isAbstract ifTrue: [^self].
	self
		addChecksWithSelectors: checkSelectorInclusionSet
		startingAtChecklist: aChecklistClass
		inTheContextOf: aChecklistClass! !

!Assessment methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checks

	^checks! !

!Assessment methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^prerequisites! !

!Assessment methodsFor: 'private - accessing' stamp: 'sqr 1/14/2016 01:43'!
addChecklist: aChecklistClass
inTheContextOf: originalChecklistClass
	"The unique check closure is necessary for checklists that inherit check selectors"

	| uniqueChecks uniqueSet |
	uniqueChecks := OrderedCollection new: self checksPerChecklistGuesstimate.
	uniqueSet := Set new: self checksPerChecklistGuesstimate.
	self
		addChecklist: aChecklistClass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks  
		uniqueSet: uniqueSet.
	self
		addUniqueChecks: uniqueChecks
		fromChecklist: aChecklistClass! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecklist: aChecklistClass
inTheContextOf: originalChecklistClass
to: uniqueChecks
uniqueSet: previouslySeenChecks

	self
		addChecksFrom: aChecklistClass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks
		uniqueSet: previouslySeenChecks.
	aChecklistClass shouldInheritChecks ifFalse: [^self].
	aChecklistClass isCheckSelectorLookupRoot ifTrue: [^self].
	self
		addChecklist: aChecklistClass checkSelectorLookupSuperclass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks
		uniqueSet: previouslySeenChecks! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecksFrom: aChecklistClass
inTheContextOf: originalChecklistClass
to: uniqueChecks
uniqueSet: previouslySeenChecks

	aChecklistClass checksDo:
		[:eachCheck |
			originalChecklistClass installContextForCheck: eachCheck.
			(previouslySeenChecks includes: eachCheck) ifFalse:
				[
					previouslySeenChecks add: eachCheck.
					uniqueChecks add: eachCheck
				]
		]! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecksWithSelectors: checkSelectorInclusionSet
from: aChecklistClass
inTheContextOf: originalChecklistClass
to: uniqueChecks  
uniqueSet: previouslySeenChecks

	aChecklistClass checksDo:
		[:eachCheck |
			(checkSelectorInclusionSet includes: eachCheck checkSelector) ifTrue:
				[
					originalChecklistClass installContextForCheck: eachCheck.
					(previouslySeenChecks includes: eachCheck) ifFalse:
						[
							previouslySeenChecks add: eachCheck.
							uniqueChecks add: eachCheck
						]
				]
		]! !

!Assessment methodsFor: 'private - accessing' stamp: 'sqr 1/14/2016 01:44'!
addChecksWithSelectors: checkSelectorInclusionSet
startingAtChecklist: aChecklistClass
inTheContextOf: originalChecklistClass
	"The unique check closure is necessary for checklists that inherit check selectors"

	| uniqueChecks uniqueSet |
	uniqueChecks := OrderedCollection new: self checksPerChecklistGuesstimate.
	uniqueSet := Set new: self checksPerChecklistGuesstimate.
	self
		addChecksWithSelectors: checkSelectorInclusionSet
		startingAtChecklist: aChecklistClass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks  
		uniqueSet: uniqueSet.
	self
		addUniqueChecks: uniqueChecks
		fromChecklist: aChecklistClass! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
addChecksWithSelectors: checkSelectors
startingAtChecklist: aChecklistClass
inTheContextOf: originalChecklistClass
to: uniqueChecks
uniqueSet: previouslySeenChecks

	self
		addChecksWithSelectors: checkSelectors
		from: aChecklistClass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks
		uniqueSet: previouslySeenChecks.
	aChecklistClass shouldInheritChecks ifFalse: [^self].
	aChecklistClass isCheckSelectorLookupRoot ifTrue: [^self].
	self
		addChecksWithSelectors: checkSelectors
		startingAtChecklist: aChecklistClass checkSelectorLookupSuperclass
		inTheContextOf: originalChecklistClass
		to: uniqueChecks
		uniqueSet: previouslySeenChecks! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
addUniqueChecks: uniqueChecks
fromChecklist: aChecklistClass
	"If there are no tests, do not query for prerequisites either.  Some
	empty SUnit test cases may implement #resources in terms of
	subclassResponsibility"

	uniqueChecks isEmpty ifTrue: [^self].
	aChecklistClass usesClassBasedPrerequisites
		ifTrue:
			[
				self privateAddChecks: uniqueChecks.
				self prerequisites addAll: aChecklistClass prerequisites
			]
		ifFalse: [self addChecks: uniqueChecks]! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checks: anObject

	checks := anObject! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites: anObject

	prerequisites := anObject! !

!Assessment methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
privateAddChecks: aCollection

	self checks addAll: aCollection! !

!Assessment methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	self checks do: aBlock! !

!Assessment methodsFor: 'private' stamp: 'sqr 1/14/2016 01:42'!
checksPerChecklistGuesstimate

	^80! !

!Assessment methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self checks: OrderedCollection new.
	self prerequisites: Set new! !

!Assessment methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
defaultEvaluationContext

	^self defaultEvaluationContextClass new! !

!Assessment methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
defaultEvaluationContextClass

	^AssessmentEvaluationContext! !

!Assessment methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluationResult

	^self defaultEvaluationContext evaluate: self! !

!Assessment methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
validationResultFor: anObject

	^self defaultEvaluationContext validate: anObject with: self! !

!Assessment methodsFor: 'converting' stamp: 'sqr 1/14/2016 01:47'!
runToFailureAssessment
	"Currently unused"

	| runToFailureChecklist |
	runToFailureChecklist := CustomRunToFailureChecklist new.
	self checksDo: [:each | runToFailureChecklist addCheck: each].
	^runToFailureChecklist assessment! !

!Assessment class methodsFor: 'instance creation'!
forChecklist: aChecklistClass

	^self new
		addChecklist: aChecklistClass;
		yourself! !

!Assessment class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:18'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!AssessmentResult methodsFor: 'private - results' stamp: 'sqr 1/14/2016 01:52'!
addResult: aCheckResult

	| taggedResults |
	taggedResults := self results
		at: aCheckResult classificationTag
		ifAbsentPut: [self emptyTaggedResults].
	taggedResults add: aCheckResult! !

!AssessmentResult methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
resultsDo: aBlock

	self results do: [:resultsByType | resultsByType do: aBlock]! !

!AssessmentResult methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
tagsAndResultsDo: aBlock
	"Do not make explicit reference to tag types in aBlock"

	self results keysAndValuesDo: aBlock! !

!AssessmentResult methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
allChecksPassed

	self tagsAndResultsDo:
		[:eachTag :someResults |
			(eachTag preventsAssessmentSuccess
				and: [someResults notEmpty])
					ifTrue: [^false]
		].
	^true! !

!AssessmentResult methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
hasErrors

	^self errors notEmpty! !

!AssessmentResult methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
hasFailures

	^self failures notEmpty
		or: [self validationFailures notEmpty
		or: [self benchmarkExpectationsFailed notEmpty]]! !

!AssessmentResult methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
hasNotices

	^self notices notEmpty! !

!AssessmentResult methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
isEmpty

	^self size = 0! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationFailedTag

	^CheckBenchmarkExpectationFailure! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationMetTag

	^CheckBenchmarkExpectationPass! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
benchmarkPassTag

	^CheckBenchmarkPass! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
benchmarkResultTag

	^CheckBenchmarkResult! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
errorTag

	^CheckError! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
failureTag

	^CheckFailure! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
noticeTag

	^CheckNotice! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
passTag

	^CheckPass! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStartFailureTag

	^PrerequisiteStartFailure! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStopFailureTag

	^PrerequisiteStopFailure! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
skipTag

	^CheckSkip! !

!AssessmentResult methodsFor: 'private - tags' stamp: 'SqR 3/8/2015 18:10'!
validationFailureTag

	^CheckValidationFailure! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationsFailed

	^self checkResultsClassifiedAs: self benchmarkExpectationFailedTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationsMet

	^self checkResultsClassifiedAs: self benchmarkExpectationMetTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
benchmarkPasses

	^self checkResultsClassifiedAs: self benchmarkPassTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
benchmarkResults

	^self checkResultsClassifiedAs: self benchmarkResultTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
errors

	^self checkResultsClassifiedAs: self errorTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
failures

	^self checkResultsClassifiedAs: self failureTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
notices

	^self checkResultsClassifiedAs: self noticeTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
passes

	^self checkResultsClassifiedAs: self passTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStartFailures

	^self checkResultsClassifiedAs: self prerequisiteStartFailureTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStopFailures

	^self checkResultsClassifiedAs: self prerequisiteStopFailureTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
skips

	^self checkResultsClassifiedAs: self skipTag! !

!AssessmentResult methodsFor: 'results' stamp: 'SqR 3/8/2015 18:10'!
validationFailures

	^self checkResultsClassifiedAs: self validationFailureTag! !

!AssessmentResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	| answer |
	answer := 0.
	self resultsDo: [:each | answer := answer + each checkExecutionCount].
	^answer! !

!AssessmentResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkResultsClassifiedAs: aTag

	^self results
		at: aTag
		ifAbsent: [self emptyTaggedResults]! !

!AssessmentResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
results

	^results! !

!AssessmentResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
size

	^self results
		inject: 0
		into: [:total :taggedResults | total + taggedResults size]! !

!AssessmentResult methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
emptyTaggedResults

	^OrderedCollection new! !

!AssessmentResult methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self results: Dictionary new! !

!AssessmentResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
results: anObject

	results := anObject! !

!AssessmentResult class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:18'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!CheckRepository methodsFor: 'private' stamp: 'SqR 3/8/2015 18:05'!
initialize

	^self! !

!CheckRepository class methodsFor: 'selector lookup'!
atSelector: aSelector

	^self methodDictionary
		at: aSelector
		ifAbsent: [nil]! !

!CheckRepository class methodsFor: 'selector lookup'!
isKeywordSelector: aSelector

	^aSelector notEmpty and: [aSelector last = $:]! !

!CheckRepository class methodsFor: 'selector lookup'!
isNotKeywordSelector: aSelector

	^(self isKeywordSelector: aSelector) not! !

!CheckRepository class methodsFor: 'selector lookup'!
selector: aSelector beginsWith: aPrefix
	"This should be SequenceableCollection>>beginsWith:"

	aSelector size < aPrefix size ifTrue: [^false].
	1 to: aPrefix size do:
		[:eachIndex |
			(aSelector at: eachIndex) = (aPrefix at: eachIndex)
				ifFalse: [^false]
		].
	^true! !

!CheckRepository class methodsFor: 'classification'!
abstractHierarchyRoot
	"The root of the current abstract hierarchy"

	^CheckRepository! !

!CheckRepository class methodsFor: 'classification'!
isAbstract
	"Abstract checklists exist for the sole purpose of refactoring,
	and are never used to perform checks.

	If it is necessary to mark sub hierarchies as abstract,
	then refine the implementation of abstractHierarchyRoot"

	^self = self abstractHierarchyRoot! !

!CheckRepository class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!CheckRepository class methodsFor: 'classification'!
shouldInheritChecks
	"If true, then Checklist subclasses inherit checks from superclasses"

	^true! !

!CheckRepository class methodsFor: 'check lookup'!
allCheckSelectors

	| answer pivotClass |
	answer := self checkSelectors.
	self shouldInheritChecks ifFalse: [^answer].
	pivotClass := self.
	[pivotClass isCheckSelectorLookupRoot] whileFalse:
		[
			pivotClass := pivotClass checkSelectorLookupSuperclass.
			pivotClass checkSelectorsDo: [:each | answer add: each]
		].
	^answer! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorDefinitionClass

	^self! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorLookupRoot
	"Answer the class past which check selectors
	are assumed to not exist.  Refine this as needed
	for performance reasons"

	^self abstractHierarchyRoot! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorLookupSubclasses

	^self subclasses! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorLookupSuperclass

	^self superclass! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorPrefix
	"Instance message selectors beginning with this
	prefix are interpreted to be check entry points"

	^nil! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectors

	| answer |
	answer := Set new.
	self checkSelectorsDo: [:each | answer add: each].
	^answer! !

!CheckRepository class methodsFor: 'check lookup'!
checkSelectorsDo: aBlock

	self selectorsDo:
		[:each |
			(self isCheckSelector: each)
				ifTrue: [aBlock value: each]
		]! !

!CheckRepository class methodsFor: 'check lookup'!
isCheckSelector: aSelector

	^(self isNotKeywordSelector: aSelector)
		and: [self selector: aSelector beginsWith: self checkSelectorPrefix]! !

!CheckRepository class methodsFor: 'check lookup'!
isCheckSelectorLookupRoot

	^self = self checkSelectorLookupRoot! !

!CheckRepository class methodsFor: 'private - runtime packager'!
allDynamicallySentSelectorsDo: aBlock

	self checkSelectorPrefix isNil ifTrue: [^self].
	self checkSelectorsDo: aBlock! !

!CheckRepository class methodsFor: 'assessments' stamp: 'SqR 2/21/2015 19:54'!
assessment

	^self assessmentClass forChecklist: self! !

!CheckRepository class methodsFor: 'assessments' stamp: 'SqR 2/21/2015 19:54'!
evaluationResult

	^self assessment evaluationResult! !

!CheckRepository class methodsFor: 'assessments' stamp: 'SqR 2/21/2015 19:53'!
installContextForCheck: aCheck

	aCheck checklist: self! !

!CheckRepository class methodsFor: 'constants'!
assessmentClass

	^Assessment! !

!CheckRepository class methodsFor: 'constants'!
checkClass

	^Check! !

!CheckRepository class methodsFor: 'check services'!
checklistForClass: aClass

	self == aClass ifTrue: [^self].
	self checkSelectorLookupSubclasses do:
		[:one |
			| checklistForClass |
			checklistForClass := one checklistForClass: aClass.
			checklistForClass notNil ifTrue: [^checklistForClass]
		].
	^nil! !

!CheckRepository class methodsFor: 'check services'!
checklistWithCheckSelectorsNamed: checkSelectors
toBePerformedWith: someArguments

	| answer |
	answer := self checklistWithCheckSelectorsNamed: checkSelectors.
	answer checksDo: [:each | each arguments: someArguments].
	^answer! !

!CheckRepository class methodsFor: 'custom checklists'!
checklistWithCheckSelectorsNamed: checkSelectors

	| answer |
	answer := CustomChecklist new.
	checkSelectors do:
		[:each |
			| newCheck |
			newCheck := self newCheckWithCheckSelector: each.
			answer addCheck: newCheck
		].
	^answer! !

!CheckRepository class methodsFor: 'checks'!
checks

	| answer |
	answer := OrderedCollection new.
	self checksDo: [:each | answer add: each].
	^answer! !

!CheckRepository class methodsFor: 'checks'!
checksDo: aBlock

	self checkSelectorsDo:
		[:each |
			| newCheck |
			newCheck := self newCheckWithCheckSelector: each.
			aBlock value: newCheck
		]! !

!CheckRepository class methodsFor: 'checks'!
newCheckWithCheckSelector: aSelector

	^self checkClass
		checklist: self
		checkSelector: aSelector! !

!CheckRepository class methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:29'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self classForBrowsing value: nil! !

!CheckRepository class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:18'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!CheckRepository class methodsFor: 'prerequisites'!
prerequisites
	"This is an ordered collection explicitly to allow subclasses to refine this message as follows.

	prerequisites

		^super prerequisites
			add: such;
			add: andSuch;
			yourself
	"

	^OrderedCollection new! !

!CheckRepository class methodsFor: 'prerequisites'!
usesClassBasedPrerequisites

	^true! !

!CheckRepository class methodsFor: 'lookup'!
withAllLookupSubclasses

	| answer pivotIndex |
	answer := OrderedCollection with: self.
	pivotIndex := 1.
	[pivotIndex <= answer size] whileTrue:
		[
			| pivot |
			pivot := answer at: pivotIndex.
			answer addAll: pivot checkSelectorLookupSubclasses.
			pivotIndex := pivotIndex + 1
		].
	^answer! !

!AbstractChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck

	^self! !

!AbstractChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck

	^self! !

!AbstractChecklist methodsFor: 'evaluation - assertions' stamp: 'SqR 2/22/2015 17:59'!
accept: assertionValue

	assertionValue ifTrue: [^self].
	self failureNotification signal: self isUnacceptable! !

!AbstractChecklist methodsFor: 'evaluation - assertions' stamp: 'SqR 3/8/2015 18:10'!
reject: aBoolean

	self accept: aBoolean not! !

!AbstractChecklist methodsFor: 'evaluation - assertions' stamp: 'SqR 2/22/2015 17:59'!
resumablyAccept: assertionValue

	assertionValue ifTrue: [^self].
	self resumableFailureNotification signal: self isUnacceptable! !

!AbstractChecklist methodsFor: 'evaluation - assertions' stamp: 'SqR 3/8/2015 18:10'!
resumablyReject: aBoolean

	self resumablyAccept: aBoolean not! !

!AbstractChecklist methodsFor: 'evaluation - value assertions' stamp: 'SqR 3/8/2015 18:10'!
acceptValueOf: aBlock

	self accept: aBlock value! !

!AbstractChecklist methodsFor: 'evaluation - value assertions' stamp: 'SqR 3/8/2015 18:10'!
rejectValueOf: aBlock

	self accept: aBlock value not! !

!AbstractChecklist methodsFor: 'evaluation - result adoption' stamp: 'SqR 2/22/2015 18:00'!
adoptAssessmentResult: anAssessmentResult

	| notification |
	notification := ResultAdoptionNotification new.
	notification assessmentResult: anAssessmentResult.
	notification signal! !

!AbstractChecklist methodsFor: 'evaluation - result adoption' stamp: 'SqR 3/8/2015 18:10'!
adoptResultsOfEvaluating: aChecklist

	| result |
	result := aChecklist assessment evaluationResult.
	self adoptAssessmentResult: result! !

!AbstractChecklist methodsFor: 'private - evaluation - exceptions' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluationOf: aBlock resultsIn: anException

	^[aBlock value.  false]
		on: anException
		do: [:ex | ex return: true]! !

!AbstractChecklist methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
defaultPrettyPrint

	^'Result'! !

!AbstractChecklist methodsFor: 'evaluation - exceptions' stamp: 'SqR 3/8/2015 18:10'!
evaluationOf: aBlock doesNotResultIn: anException

	self reject: (self basicEvaluationOf: aBlock resultsIn: anException)! !

!AbstractChecklist methodsFor: 'evaluation - exceptions' stamp: 'SqR 3/8/2015 18:10'!
evaluationOf: aBlock resultsIn: anException

	self accept: (self basicEvaluationOf: aBlock resultsIn: anException)! !

!AbstractChecklist methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
failureNotification

	^CheckFailureNotification! !

!AbstractChecklist methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
noticeNotification

	^CheckNoticeNotification! !

!AbstractChecklist methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
passNotification

	^CheckPassNotification! !

!AbstractChecklist methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
resumableFailureNotification

	^CheckResumableFailureNotification! !

!AbstractChecklist methodsFor: 'private - message patterns' stamp: 'SqR 3/8/2015 18:10'!
isAcceptable

	^self prettyPrint, self isAcceptableSuffix! !

!AbstractChecklist methodsFor: 'private - message patterns' stamp: 'SqR 3/8/2015 18:10'!
isAcceptableSuffix

	^' is acceptable'! !

!AbstractChecklist methodsFor: 'private - message patterns' stamp: 'SqR 3/8/2015 18:10'!
isUnacceptable

	^self prettyPrint, self isUnacceptableSuffix! !

!AbstractChecklist methodsFor: 'private - message patterns' stamp: 'SqR 3/8/2015 18:10'!
isUnacceptableSuffix

	^' is unacceptable'! !

!AbstractChecklist methodsFor: 'evaluation - notices' stamp: 'SqR 3/6/2015 20:20'!
note: aString

	| notice |
	notice := self noticeNotification new.
	notice notice: aString.
	notice signal: self prettyPrint! !

!AbstractChecklist methodsFor: 'accessing - polymorphism' stamp: 'SqR 3/8/2015 18:10'!
object

	^nil! !

!AbstractChecklist methodsFor: 'accessing - polymorphism' stamp: 'SqR 3/8/2015 18:10'!
object: anObject

	^self! !

!AbstractChecklist methodsFor: 'accessing - polymorphism' stamp: 'SqR 3/8/2015 18:10'!
originalObject

	^nil! !

!AbstractChecklist methodsFor: 'accessing - polymorphism' stamp: 'SqR 3/8/2015 18:10'!
originalObject: anObject

	^self! !

!AbstractChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint

	^prettyPrint ifNil: [self defaultPrettyPrint]! !

!AbstractChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint: anObject

	prettyPrint := anObject! !

!AbstractChecklist class methodsFor: 'classification'!
abstractHierarchyRoot

	^AbstractChecklist! !

!AbstractChecklist class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^true! !

!Benchmark methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkExpectationFailedNotification

	^CheckBenchmarkExpectationFailedNotification! !

!Benchmark methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkExpectationMetNotification

	^CheckBenchmarkExpectationMetNotification! !

!Benchmark methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkMeasurementNotification

	^CheckBenchmarkMeasurementNotification! !

!Benchmark methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkPassNotification

	^CheckBenchmarkPassNotification! !

!Benchmark methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
defaultPrettyPrint

	^'Measurement'! !

!Benchmark methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
defaultPrettyPrintForScenario: aString

	self prettyPrintForScenario notNil ifTrue: [^self].
	self prettyPrintForScenario: aString! !

!Benchmark methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario: anObject

	prettyPrintForScenario := anObject! !

!Benchmark methodsFor: 'private - memory spaces' stamp: 'sqr 1/16/2016 21:18'!
flushNativeCodeCache

	AssessmentsGC current flushNativeCodeCache! !

!Benchmark methodsFor: 'private - memory spaces' stamp: 'sqr 1/16/2016 21:18'!
flushNewSpace

	AssessmentsGC current flushNewSpace! !

!Benchmark methodsFor: 'evaluation - measurements' stamp: 'SqR 3/8/2015 18:10'!
itTakesLessThan: anInteger
messageSendsToEvaluate: aBlock

	self
		itTakesLessThan: anInteger
		messageSendsToEvaluate: aBlock
		retriesSoFar: 0! !

!Benchmark methodsFor: 'evaluation - measurements' stamp: 'SqR 3/8/2015 18:10'!
maximumRetriesAllowed

	^1! !

!Benchmark methodsFor: 'evaluation - measurements' stamp: 'SqR 3/8/2015 18:10'!
stopWatch: aBlock

	| answer |
	answer := self privateStopWatch: aBlock.
	self reportIterationsPerSecond: answer! !

!Benchmark methodsFor: 'private - evaluation - measurements' stamp: 'SqR 3/8/2015 18:10'!
itTakesLessThan: anInteger
messageSendsToEvaluate: aBlock
retriesSoFar: retriesSoFar

	| simpleMessageIterations blockIterations expectedIterations |
	simpleMessageIterations := self privateStopWatch: [self simpleMessageSend].
	expectedIterations := simpleMessageIterations / anInteger.
	blockIterations := self privateStopWatch: aBlock.
	(blockIterations < expectedIterations
		and: [retriesSoFar < self maximumRetriesAllowed])
			ifTrue:
				[
					^self
						itTakesLessThan: anInteger
						messageSendsToEvaluate: aBlock
						retriesSoFar: retriesSoFar + 1
				].
	self
		reportExpectedIterationsPerSecond: expectedIterations
		andActualIterationsPerSecond: blockIterations! !

!Benchmark methodsFor: 'private - evaluation - measurements' stamp: 'SqR 2/22/2015 18:02'!
reportExpectedIterationsPerSecond: expectedIterationsPerSecond
andActualIterationsPerSecond: iterationsPerSecond

	| notificationClass |
	notificationClass := iterationsPerSecond >= expectedIterationsPerSecond
		ifTrue: [self checkBenchmarkExpectationMetNotification]
		ifFalse: [self checkBenchmarkExpectationFailedNotification].
	notificationClass new
		iterationsPerSecond: iterationsPerSecond;
		expectedIterationsPerSecond: expectedIterationsPerSecond;
		prettyPrintForScenario: self prettyPrintForScenario;
		signal: self prettyPrint! !

!Benchmark methodsFor: 'private - evaluation - measurements' stamp: 'SqR 2/22/2015 18:02'!
reportIterationsPerSecond: aNumber

	self checkBenchmarkMeasurementNotification new
		iterationsPerSecond: aNumber;
		prettyPrintForScenario: self prettyPrintForScenario;
		signal: self prettyPrint! !

!Benchmark methodsFor: 'private - limits' stamp: 'SqR 3/8/2015 18:10'!
microsecondThreshold

	^1000000! !

!Benchmark methodsFor: 'private - limits' stamp: 'SqR 3/8/2015 18:10'!
microsecondsPerSecond

	^1000000! !

!Benchmark methodsFor: 'private - limits' stamp: 'SqR 3/8/2015 18:10'!
repetitionLimit
	"1 bitShift: 32"

	^4294967296! !

!Benchmark methodsFor: 'private - stop watch' stamp: 'SqR 3/8/2015 18:10'!
musToRun: anInteger times: aBlock
	"Avoid sending timesRepeat: to a large integer"

	| remainder answer |
	remainder := anInteger.
	answer := 0.
	self flushNewSpace.
	[remainder = 0] whileFalse:
		[
			| localRuns localAnswer |
			localRuns := remainder min: SmallInteger maxVal.
			localAnswer := Time microsecondsToRun: [localRuns timesRepeat: [aBlock value]].
			answer := answer + localAnswer.
			remainder := remainder - localRuns
		].
	^answer! !

!Benchmark methodsFor: 'private - stop watch' stamp: 'SqR 3/8/2015 18:10'!
netMusToRun: repetitions times: aBlock

	| actualMicroseconds overheadMicroseconds |
	actualMicroseconds := self musToRun: repetitions times: aBlock.
	overheadMicroseconds := self musToRun: repetitions times: [nil].
	overheadMicroseconds := overheadMicroseconds min: actualMicroseconds.
	^actualMicroseconds - overheadMicroseconds! !

!Benchmark methodsFor: 'private - stop watch' stamp: 'sqr 5/25/2016 14:21'!
privateStopWatch: aBlock
	"Answer iterations per second.  Flush compiled code cache by performing a garbage collect,
	run the block one time to get the native code generated, flush the new space if possible,
	and then and only then start measuring"

	| theseRepetitions nextRepetitions netMicroseconds |
	self flushNativeCodeCache.
	aBlock value.
	netMicroseconds := 0.
	nextRepetitions := 1.
	[
		netMicroseconds > self microsecondThreshold
			or: [nextRepetitions > self repetitionLimit]
	] whileFalse:
		[
			| repetitionMultiplier |
			theseRepetitions := nextRepetitions.
			netMicroseconds := self netMusToRun: theseRepetitions times: aBlock.
			repetitionMultiplier := self repetitionMultiplierBasedOn: netMicroseconds.
			nextRepetitions := (theseRepetitions * repetitionMultiplier) truncated
				max: theseRepetitions + 1
		].
	^(theseRepetitions * self microsecondsPerSecond) asFloat / (netMicroseconds max: 1)! !

!Benchmark methodsFor: 'private - stop watch' stamp: 'SqR 2/22/2015 19:26'!
repetitionMultiplierBasedOn: netMicroseconds

	| basicFactor antiAliasedNetMicroseconds |
	antiAliasedNetMicroseconds := netMicroseconds max: 1000.
	basicFactor := self microsecondThreshold / antiAliasedNetMicroseconds.
	^basicFactor * 1.1! !

!Benchmark methodsFor: 'private - stop watch' stamp: 'SqR 3/8/2015 18:10'!
simpleMessageSend

	^self! !

!Benchmark methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario

	^prettyPrintForScenario! !

!Benchmark class methodsFor: 'classification'!
abstractHierarchyRoot

	^Benchmark! !

!Benchmark class methodsFor: 'private - runtime packager'!
allDynamicallySentSelectorsDo: aBlock

	super allDynamicallySentSelectorsDo: aBlock.
	self allScenarioSelectors do: aBlock! !

!Benchmark class methodsFor: 'scenario lookup'!
allScenarioSelectors

	| pivotClass answer |
	answer := Set new.
	pivotClass := self.
	[
		pivotClass scenarioSelectorsDo: [:each | answer add: each].
		pivotClass = self checkSelectorLookupRoot ifTrue: [^answer asSortedCollection].
		pivotClass := pivotClass checkSelectorLookupSuperclass
	] repeat! !

!Benchmark class methodsFor: 'scenario lookup'!
checkScenarioPrefix

	^'scenario'! !

!Benchmark class methodsFor: 'scenario lookup'!
defaultPrettyPrintForScenario: aScenarioSelector

	^aScenarioSelector
		copyFrom: self checkScenarioPrefix size + 1
		to: aScenarioSelector size! !

!Benchmark class methodsFor: 'scenario lookup'!
isScenarioSelector: aSelector

	^(self isNotKeywordSelector: aSelector)
		and: [self selector: aSelector beginsWith: self checkScenarioPrefix]! !

!Benchmark class methodsFor: 'scenario lookup'!
scenarioSelectorsDo: aBlock

	self selectorsDo:
		[:each |
			(self isScenarioSelector: each) ifTrue: [aBlock value: each]
		]! !

!Benchmark class methodsFor: 'check lookup'!
checkSelectorPrefix

	^'measure'! !

!Benchmark class methodsFor: 'check services'!
checklistForClass: aClass

	self = self abstractHierarchyRoot ifTrue:
		[
			self = aClass ifTrue: [^aClass].
			^(aClass inheritsFrom: self)
				ifTrue: [aClass] ifFalse: [nil]
		].
	^super checklistForClass: aClass! !

!Benchmark class methodsFor: 'checks'!
newCheckWithCheckSelector: aSelector

	| answer |
	answer := super newCheckWithCheckSelector: aSelector.
	answer useAssessmentsBenchmarkExecutionPolicy.
	^answer! !

!BenchmarkMetaSelfChecklist methodsFor: 'checks - helpers' stamp: 'SqR 3/8/2015 18:10'!
divideByZero

	1 / 0! !

!BenchmarkMetaSelfChecklist class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!BenchmarkChecklist methodsFor: 'measurements' stamp: 'sqr 1/13/2016 22:24'!
measureSomeExpectationToFail

	self prettyPrint: 'Slow code'.
	self
		itTakesLessThan: 1
		messageSendsToEvaluate: [10 factorial]! !

!BenchmarkChecklist methodsFor: 'measurements' stamp: 'sqr 1/13/2016 22:25'!
measureSomeExpectationToMeet

	self prettyPrint: 'Fast code'.
	self
		itTakesLessThan: 1000
		messageSendsToEvaluate: [10 factorial]! !

!BenchmarkChecklist methodsFor: 'measurements' stamp: 'SqR 3/8/2015 18:10'!
measureSomething

	self prettyPrint: '2 + 3'.
	self stopWatch: [2 + 3]! !

!BenchmarkChecklist methodsFor: 'measurements' stamp: 'SqR 3/8/2015 18:10'!
measureSomethingWithoutPrettyPrint

	self stopWatch: [2 + 3]! !

!BenchmarkChecklist methodsFor: 'scenarios' stamp: 'SqR 3/8/2015 18:10'!
scenarioUnique

	^self! !

!Checklist class methodsFor: 'classification'!
abstractHierarchyRoot

	^Checklist! !

!Checklist class methodsFor: 'check lookup'!
checkSelectorPrefix

	^'check'! !

!Checklist class methodsFor: 'check services'!
checklistForClass: aClass

	self = self abstractHierarchyRoot ifTrue:
		[
			self = aClass ifTrue: [^aClass].
			^(aClass inheritsFrom: self)
				ifTrue: [aClass] ifFalse: [nil]
		].
	^super checklistForClass: aClass! !

!AssessmentsMetaSelfChecklist methodsFor: 'checks - helpers' stamp: 'SqR 3/8/2015 18:10'!
divideByZero

	1 / 0! !

!AssessmentsMetaSelfChecklist class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!AssessmentsAcceptChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAccept

	self accept: 1 + 1 = 2! !

!AssessmentsAcceptChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptFailure

	self accept: 1 + 1 = 3! !

!AssessmentsResumablyAcceptChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAccept

	self resumablyAccept: 1 + 1 = 2! !

!AssessmentsResumablyAcceptChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptFailure

	self resumablyAccept: 1 + 1 = 3.
	self divideByZero! !

!AssessmentsAcceptValueOfChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptValueOf

	self acceptValueOf: [1 + 1 = 2]! !

!AssessmentsAcceptValueOfChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptValueOfFailure

	self acceptValueOf: [1 + 1 = 3]! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck

	self aboutToEvaluateCheckSent: true! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck

	self aboutToEvaluateCheckSent: false! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheckSent

	^aboutToEvaluateCheckSent! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheckSent: anObject

	aboutToEvaluateCheckSent := anObject! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAboutToEvaluateCheckSent

	self accept: self aboutToEvaluateCheckSent! !

!AssessmentsCheckExecutionSetUpChecklist methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self aboutToEvaluateCheckSent: false! !

!AssessmentsCheckExecutionSetUpFailureChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck

	super aboutToEvaluateCheck.
	self divideByZero! !

!AssessmentsCheckExecutionTearDownChecklist methodsFor: 'evaluation' stamp: 'SqR 2/22/2015 19:16'!
doneEvaluatingCheck

	super doneEvaluatingCheck.
	self divideByZero! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptDefaultPrettyPrint

	self accept: 1 + 1 = 2! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptFailureDefaultPrettyPrint

	self accept: 1 + 1 = 3! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectDefaultPrettyPrint

	self reject: 1 + 1 = 3! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectFailureDefaultPrettyPrint

	self reject: 1 + 1 = 2! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyAcceptDefaultPrettyPrint

	self resumablyAccept: 1 + 1 = 2! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyAcceptFailureDefaultPrettyPrint

	self resumablyAccept: 1 + 1 = 3! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyRejectDefaultPrettyPrint

	self resumablyReject: 1 + 1 = 3! !

!AssessmentsDefaultPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyRejectFailureDefaultPrettyPrint

	self resumablyReject: 1 + 1 = 2! !

!AssessmentsEvaluationOfDoesNotResultInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfDoesNotResultIn

	self
		evaluationOf: [3 + 4]
		doesNotResultIn: MessageNotUnderstood! !

!AssessmentsEvaluationOfDoesNotResultInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfDoesNotResultInError

	self
		evaluationOf: [3 / 0]
		doesNotResultIn: MessageNotUnderstood! !

!AssessmentsEvaluationOfDoesNotResultInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfDoesNotResultInFailure

	self
		evaluationOf: [self divideByZero]
		doesNotResultIn: ZeroDivide! !

!AssessmentsEvaluationOfResultsInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfResultsIn

	self
		evaluationOf: [self divideByZero]
		resultsIn: ZeroDivide! !

!AssessmentsEvaluationOfResultsInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfResultsInError

	self
		evaluationOf: [self divideByZero]
		resultsIn: MessageNotUnderstood! !

!AssessmentsEvaluationOfResultsInChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfResultsInFailure

	self
		evaluationOf: [3 + 4]
		resultsIn: MessageNotUnderstood! !

!AssessmentsNoOpCheckChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkNothing

	^self! !

!AssessmentsNoticeChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkMoreThanOneNoticePerCheck

	self note: '1'.
	self note: '2'! !

!AssessmentsNotifyingPrerequisiteChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkPrerequisiteStarted

	self accept: self class prerequisiteStarted! !

!AssessmentsNotifyingPrerequisiteChecklist class methodsFor: 'notifying prerequisite'!
newNotifyingPrerequisite

	self prerequisiteStarted: false.
	^NotifyingChecklistPrerequisite new
		when: #prerequisiteStarted send: #prerequisiteStartedNotification to: self;
		yourself! !

!AssessmentsNotifyingPrerequisiteChecklist class methodsFor: 'notifying prerequisite'!
prerequisiteStartedNotification

	self prerequisiteStarted: true! !

!AssessmentsNotifyingPrerequisiteChecklist class methodsFor: 'accessing'!
prerequisiteStarted

	^prerequisiteStarted! !

!AssessmentsNotifyingPrerequisiteChecklist class methodsFor: 'accessing'!
prerequisiteStarted: aBoolean

	prerequisiteStarted := aBoolean! !

!AssessmentsNotifyingPrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: self newNotifyingPrerequisite;
		yourself! !

!AssessmentsPrettyPrintChecklist methodsFor: 'strings' stamp: 'SqR 3/8/2015 18:10'!
acceptPrettyPrint

	^'1 + 1 = 2'! !

!AssessmentsPrettyPrintChecklist methodsFor: 'strings' stamp: 'SqR 3/8/2015 18:10'!
rejectPrettyPrint

	^'1 + 1 = 3'! !

!AssessmentsPrettyPrintChecklist methodsFor: 'strings' stamp: 'SqR 3/8/2015 18:10'!
resumablyAcceptPrettyPrint

	^self acceptPrettyPrint, ' resumably'! !

!AssessmentsPrettyPrintChecklist methodsFor: 'strings' stamp: 'SqR 3/8/2015 18:10'!
resumablyRejectPrettyPrint

	^self rejectPrettyPrint, ' resumably'! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAccept

	self prettyPrint: self acceptPrettyPrint.
	self accept: 1 + 1 = 2! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptFailure

	self prettyPrint: self acceptPrettyPrint.
	self accept: 1 + 1 = 3! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkReject

	self prettyPrint: self rejectPrettyPrint.
	self reject: 1 + 1 = 3! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectFailure

	self prettyPrint: self rejectPrettyPrint.
	self reject: 1 + 1 = 2! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyAccept

	self prettyPrint: self resumablyAcceptPrettyPrint.
	self resumablyAccept: 1 + 1 = 2! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyAcceptFailure

	self prettyPrint: self resumablyAcceptPrettyPrint.
	self resumablyAccept: 1 + 1 = 3! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyReject

	self prettyPrint: self resumablyRejectPrettyPrint.
	self resumablyReject: 1 + 1 = 3! !

!AssessmentsPrettyPrintChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyRejectFailure

	self prettyPrint: self resumablyRejectPrettyPrint.
	self resumablyReject: 1 + 1 = 2! !

!AssessmentsRejectChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkReject

	self reject: 1 + 1 = 3! !

!AssessmentsRejectChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectFailure

	self reject: 1 + 1 = 2! !

!AssessmentsResumablyRejectChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkReject

	self resumablyReject: 1 + 1 = 3! !

!AssessmentsResumablyRejectChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectFailure

	self resumablyReject: 1 + 1 = 2.
	self divideByZero! !

!AssessmentsRejectValueOfChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectValueOf

	self rejectValueOf: [1 + 1 = 3]! !

!AssessmentsRejectValueOfChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectValueOfFailure

	self rejectValueOf: [1 + 1 = 2]! !

!AssessmentsSimplePrerequisiteChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkNothing

	^self! !

!AssessmentsSimplePrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: AbstractPrerequisite new;
		yourself! !

!AssessmentsNonStartablePrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: NonStartableChecklistPrerequisite new;
		yourself! !

!AssessmentsNonStoppablePrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: NonStoppableChecklistPrerequisite new;
		yourself! !

!AssessmentsStartErrorPrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: StartErrorChecklistPrerequisite new;
		yourself! !

!AssessmentsStopErrorPrerequisiteChecklist class methodsFor: 'prerequisites'!
prerequisites

	^super prerequisites
		add: StopErrorChecklistPrerequisite new;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
defaultEvaluationContext

	^AssessmentEvaluationContext new
		evaluationPolicy: DefaultAssessmentEvaluationPolicy new;
		prerequisitePolicy: DefaultAssessmentPrerequisitePolicy new;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateChecklist: aChecklist

	^self evaluationContext evaluate: aChecklist assessment! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext

	^self defaultEvaluationContext! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassBufferedFileLogEvaluationContext

	| resultPolicy |
	resultPolicy := CompositeAssessmentResultPolicy new.
	resultPolicy addPolicy: (BufferedFileLogAssessmentResultPolicy forFileNamed: self logFileName).
	^self fastPassEvaluationContext
		resultPolicy: resultPolicy;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassEvaluationContext

	^self defaultEvaluationContext
		evaluationPolicy: FastPassAssessmentEvaluationPolicy new;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassFileLogEvaluationContext

	| resultPolicy |
	resultPolicy := CompositeAssessmentResultPolicy new.
	resultPolicy addPolicy: (FileLogAssessmentResultPolicy forFileNamed: self logFileName).
	^self fastPassEvaluationContext
		resultPolicy: resultPolicy;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassHardenedFileLogEvaluationContext

	| resultPolicy |
	resultPolicy := CompositeAssessmentResultPolicy new.
	resultPolicy addPolicy: (HardenedFileLogAssessmentResultPolicy forFileNamed: self logFileName).
	^self fastPassEvaluationContext
		resultPolicy: resultPolicy;
		yourself! !

!AbstractSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
logFileName

	^'assessmentSelfTest.txt'! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'sqr 1/26/2016 09:07'!
checkSimpleCodeMetrics

	| numberOfClassesAndExtensions numberOfMethods methodsPerClassAverage |
	AssessmentsSystemOrganization current isSupported
		ifFalse: [self note: 'Code self measurement unsupported in this dialect.'.  ^self].
	numberOfClassesAndExtensions := self numberOfClassesAndExtensions.
	numberOfMethods := self numberOfMethods.
	methodsPerClassAverage := numberOfMethods asFloat / numberOfClassesAndExtensions.
	self note:
		'There are ',
		numberOfClassesAndExtensions printString,
		' classes and class extensions.'.
	self note:
		'There are ',
		numberOfMethods printString,
		' methods.'.
	self note:
		'There are ',
		methodsPerClassAverage printString,
		' methods per class on average.'! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'sqr 1/16/2016 21:48'!
classes

	^AssessmentsSystemOrganization current classes! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/6/2015 20:16'!
numberOfClasses

	^self classes size! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/8/2015 18:10'!
numberOfClassesAndExtensions

	^self numberOfClasses + self numberOfExtensions! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/8/2015 18:10'!
numberOfExtensionMethods

	^22! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/8/2015 18:10'!
numberOfExtensions

	^9! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/8/2015 18:10'!
numberOfMethods

	^self numberOfMethodsInAssessments
		+ self numberOfExtensionMethods! !

!AssessmentsCodeMetricsChecklist methodsFor: 'metrics' stamp: 'SqR 3/6/2015 20:17'!
numberOfMethodsInAssessments

	^self classes
		inject: 0
		into: [:total :each | total + each selectors size + each class selectors size]! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptChecklist

	| result |
	result := self evaluateChecklist: AssessmentsAcceptChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkAccept.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkAcceptFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAcceptValueOfChecklist

	| result |
	result := self evaluateChecklist: AssessmentsAcceptValueOfChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkAcceptValueOf.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkAcceptValueOfFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAssessmentsCheckExecutionSetUpChecklist

	| result |
	result := self evaluateChecklist: AssessmentsCheckExecutionSetUpChecklist.
	self accept: result allChecksPassed.
	self accept: result size = 1.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkAboutToEvaluateCheckSent! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAssessmentsCheckExecutionSetUpFailureChecklist

	| result |
	result := self evaluateChecklist: AssessmentsCheckExecutionSetUpFailureChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 1.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkAboutToEvaluateCheckSent! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 2/22/2015 19:11'!
checkAssessmentsCheckExecutionTearDownChecklist

	| result |
	result := self evaluateChecklist: AssessmentsCheckExecutionTearDownChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkAboutToEvaluateCheckSent.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkAboutToEvaluateCheckSent! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkAssessmentsNotifyingPrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsNotifyingPrerequisiteChecklist.
	self accept: result allChecksPassed.
	self accept: result size = 1.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkPrerequisiteStarted! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkDefaultPrettyPrintChecklist

	| result checkResult |
	result := self evaluateChecklist: AssessmentsDefaultPrettyPrintChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 8.
	self accept: result passes size = 4.
	self accept: result failures size = 4.
	checkResult := result passes detect: [:one | one checkSelector = #checkAcceptDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isAcceptable.
	checkResult := result passes detect: [:one | one checkSelector = #checkRejectDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isAcceptable.
	checkResult := result passes detect: [:one | one checkSelector = #checkResumablyAcceptDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isAcceptable.
	checkResult := result passes detect: [:one | one checkSelector = #checkResumablyRejectDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isAcceptable.
	checkResult := result failures detect: [:one | one checkSelector = #checkAcceptFailureDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isUnacceptable.
	checkResult := result failures detect: [:one | one checkSelector = #checkRejectFailureDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isUnacceptable.
	checkResult := result failures detect: [:one | one checkSelector = #checkResumablyAcceptFailureDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isUnacceptable.
	checkResult := result failures detect: [:one | one checkSelector = #checkResumablyRejectFailureDefaultPrettyPrint].
	self accept: checkResult prettyPrint = AssessmentsPrettyPrintChecklist new isUnacceptable! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfDoesNotResultInChecklist

	| result |
	result := self evaluateChecklist: AssessmentsEvaluationOfDoesNotResultInChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkEvaluationOfDoesNotResultIn.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkEvaluationOfDoesNotResultInFailure.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkEvaluationOfDoesNotResultInError! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkEvaluationOfResultsInChecklist

	| result |
	result := self evaluateChecklist: AssessmentsEvaluationOfResultsInChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkEvaluationOfResultsIn.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkEvaluationOfResultsInFailure.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkEvaluationOfResultsInError! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkNoOpCheckChecklist

	| result |
	result := self evaluateChecklist: AssessmentsNoOpCheckChecklist.
	self accept: result allChecksPassed.
	self accept: result size = 1.
	self accept: result passes size = 1! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkNonStartablePrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsNonStartablePrerequisiteChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 1.
	self accept: result passes isEmpty.
	self accept: result prerequisiteStartFailures size = 1.
	self accept: (result prerequisiteStartFailures first prerequisite receiver isKindOf: NonStartableChecklistPrerequisite)! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkNonStoppablePrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsNonStoppablePrerequisiteChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result prerequisiteStopFailures size = 1.
	self accept: (result prerequisiteStopFailures first prerequisite receiver isKindOf: NonStoppableChecklistPrerequisite)! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkPrettyPrintChecklist

	| result checkResult |
	result := self evaluateChecklist: AssessmentsPrettyPrintChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 8.
	self accept: result passes size = 4.
	self accept: result failures size = 4.
	checkResult := result passes detect: [:one | one checkSelector = #checkAccept].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new acceptPrettyPrint, ' is acceptable').
	checkResult := result passes detect: [:one | one checkSelector = #checkReject].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new rejectPrettyPrint, ' is acceptable').
	checkResult := result passes detect: [:one | one checkSelector = #checkResumablyAccept].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new resumablyAcceptPrettyPrint, ' is acceptable').
	checkResult := result passes detect: [:one | one checkSelector = #checkResumablyReject].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new resumablyRejectPrettyPrint, ' is acceptable').
	checkResult := result failures detect: [:one | one checkSelector = #checkAcceptFailure].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new acceptPrettyPrint, ' is unacceptable').
	checkResult := result failures detect: [:one | one checkSelector = #checkRejectFailure].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new rejectPrettyPrint, ' is unacceptable').
	checkResult := result failures detect: [:one | one checkSelector = #checkResumablyAcceptFailure].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new resumablyAcceptPrettyPrint, ' is unacceptable').
	checkResult := result failures detect: [:one | one checkSelector = #checkResumablyRejectFailure].
	self accept: checkResult prettyPrint = (AssessmentsPrettyPrintChecklist new resumablyRejectPrettyPrint, ' is unacceptable')! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectChecklist

	| result |
	result := self evaluateChecklist: AssessmentsRejectChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkReject.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkRejectFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkRejectValueOfChecklist

	| result |
	result := self evaluateChecklist: AssessmentsRejectValueOfChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkRejectValueOf.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkRejectValueOfFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyAcceptChecklist

	| result |
	result := self evaluateChecklist: AssessmentsResumablyAcceptChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkAccept.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkAcceptFailure.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkAcceptFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResumablyRejectChecklist

	| result |
	result := self evaluateChecklist: AssessmentsResumablyRejectChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkReject.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkRejectFailure.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkRejectFailure! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkSimplePrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsSimplePrerequisiteChecklist.
	self accept: result allChecksPassed.
	self accept: result size = 1.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkNothing! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkStartErrorPrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsStartErrorPrerequisiteChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 1.
	self accept: result passes isEmpty.
	self accept: result prerequisiteStartFailures size = 1.
	self accept: (result prerequisiteStartFailures first prerequisite receiver isKindOf: StartErrorChecklistPrerequisite)! !

!AssessmentsDefaultEvaluationContextChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkStopErrorPrerequisiteChecklist

	| result |
	result := self evaluateChecklist: AssessmentsStopErrorPrerequisiteChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 2.
	self accept: result passes size = 1.
	self accept: result prerequisiteStopFailures size = 1.
	self accept: (result prerequisiteStopFailures first prerequisite receiver isKindOf: StopErrorChecklistPrerequisite)! !

!AssessmentsFastPassEvaluationPolicyChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck

	super aboutToEvaluateCheck.
	self prettyPrint: '(this should appear 3 times) ', super prettyPrint! !

!AssessmentsFastPassEvaluationPolicyChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext

	^self fastPassEvaluationContext! !

!AssessmentsManualResultAdoptionChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResultAdoption

	| result |
	result := self evaluateChecklist: AssessmentsFastPassEvaluationPolicyChecklist.
	self adoptAssessmentResult: result! !

!AssessmentsAutomaticResultAdoptionChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkResultAdoption

	self adoptResultsOfEvaluating: AssessmentsFastPassEvaluationPolicyChecklist! !

!AssessmentsSelfVerificationChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck

	super aboutToEvaluateCheck.
	self deleteAnyLogFilePresent! !

!AssessmentsSelfVerificationChecklist methodsFor: 'evaluation' stamp: 'SqR 11/18/2015 15:57'!
deleteAnyLogFilePresent

	(AssessmentsFileDirectory current fileNameExists: self logFileName) ifFalse: [^self].
	AssessmentsFileDirectory current deleteFileNamed: self logFileName! !

!AssessmentsSelfVerificationChecklist methodsFor: 'evaluation' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck

	super doneEvaluatingCheck.
	self deleteAnyLogFilePresent! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkAutomaticResultAdoptionChecklist

	self verifyResultAdoptionChecklist: AssessmentsAutomaticResultAdoptionChecklist! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkCustomChecklist

	| customChecklist expectedResult actualResult |
	customChecklist := CustomChecklist new.
	customChecklist addChecklist: AssessmentsDefaultPrettyPrintChecklist.
	expectedResult := self evaluateChecklist: AssessmentsDefaultPrettyPrintChecklist.
	actualResult := self evaluateChecklist: customChecklist.
	self accept: actualResult allChecksPassed = expectedResult allChecksPassed.
	self accept: actualResult size = expectedResult size.
	self accept: actualResult passes size = expectedResult passes size.
	self accept: actualResult failures size = expectedResult failures size.
	self accept: actualResult errors size = expectedResult errors size! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkDefaultEvaluationContextChecklist

	| result |
	result := self evaluateChecklist: AssessmentsDefaultEvaluationContextChecklist.
	self accept: result allChecksPassed! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkEmptyAbstractCustomChecklist

	| result |
	result := self evaluateChecklist: AbstractCustomChecklist new.
	self accept: result allChecksPassed! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkEmptyCustomChecklist

	| result |
	result := self evaluateChecklist: CustomChecklist new.
	self accept: result allChecksPassed! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 2/22/2015 20:27'!
checkFastPassBufferedFileLogEvaluationPolicyChecklist

	| result |
	result := self fastPassBufferedFileLogEvaluationContext evaluate: AssessmentsFastPassEvaluationPolicyChecklist assessment.
	self accept: result allChecksPassed.
	self verifyLogFile! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkFastPassEvaluationPolicyChecklist

	| result |
	result := self evaluateChecklist: AssessmentsFastPassEvaluationPolicyChecklist.
	self accept: result allChecksPassed! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 2/22/2015 20:27'!
checkFastPassFileLogEvaluationPolicyChecklist

	| result |
	result := self fastPassFileLogEvaluationContext evaluate: AssessmentsFastPassEvaluationPolicyChecklist assessment.
	self accept: result allChecksPassed.
	self verifyLogFile! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/6/2015 19:59'!
checkFastPassHardenedFileLogEvaluationPolicyChecklist

	| result |
	result := self fastPassHardenedFileLogEvaluationContext evaluate: AssessmentsFastPassEvaluationPolicyChecklist assessment.
	self accept: result allChecksPassed.
	self verifyLogFile! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkManualResultAdoptionChecklist

	self verifyResultAdoptionChecklist: AssessmentsManualResultAdoptionChecklist! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
checkNoticeChecklist

	| result |
	result := self evaluateChecklist: AssessmentsNoticeChecklist.
	self accept: result allChecksPassed.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result notices size = 2! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 11/18/2015 16:01'!
verifyLogFile

	self accept: (AssessmentsFileDirectory current fileNameExists: self logFileName).
	self accept: (AssessmentsFileDirectory current sizeForFileName: self logFileName) > 0! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checks - assessments' stamp: 'SqR 3/8/2015 18:10'!
verifyResultAdoptionChecklist: aChecklistClass

	| resultWithoutAdoption resultWithAdoption |
	resultWithoutAdoption := self evaluateChecklist: AssessmentsDefaultEvaluationContextChecklist.
	resultWithAdoption := self evaluateChecklist: aChecklistClass.
	self accept: resultWithoutAdoption allChecksPassed.
	self accept: resultWithAdoption allChecksPassed.
	self accept: resultWithoutAdoption passes size + 1 = resultWithAdoption passes size.
	self accept: (resultWithoutAdoption passes allSatisfy: [:one | resultWithAdoption passes anySatisfy: [:some | some checkSelector = one checkSelector]]).
	self accept: (resultWithAdoption passes anySatisfy: [:one | one checkSelector = #checkResultAdoption])! !

!AssessmentsSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluationContext

	^self fastPassEvaluationContext! !

!AssessmentsServicesChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkChecklistForClass

	self accept: (CheckRepository checklistForClass: CheckRepository) == CheckRepository.
	self accept: (CheckRepository checklistForClass: Validator) == Validator! !

!AssessmentsServicesChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkService

	| serviceChecklist result |
	serviceChecklist := self class checkServiceFor: self objectToRunServiceOn.
	result := self evaluateChecklist: serviceChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 3.
	self accept: result passes size = 1.
	self accept: result passes first checkSelector = #checkServicePass:.
	self accept: result failures size = 1.
	self accept: result failures first checkSelector = #checkServiceFail:.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #checkServiceError:! !

!AssessmentsServicesChecklist methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checkServiceResistsDNU

	| serviceChecklist result |
	serviceChecklist := self class checklistWithCheckSelectorsNamed: #(#shouldNotUnderstand).
	result := self evaluateChecklist: serviceChecklist.
	self accept: result allChecksPassed not.
	self accept: result size = 1.
	self accept: result errors size = 1.
	self accept: result errors first checkSelector = #shouldNotUnderstand! !

!AssessmentsServicesChecklist methodsFor: 'checks - services' stamp: 'SqR 3/8/2015 18:10'!
checkServiceError: anObject

	1 / 0! !

!AssessmentsServicesChecklist methodsFor: 'checks - services' stamp: 'SqR 3/8/2015 18:10'!
checkServiceFail: anObject

	self accept: self objectToRunServiceOn = self! !

!AssessmentsServicesChecklist methodsFor: 'checks - services' stamp: 'SqR 3/8/2015 18:10'!
checkServicePass: anObject

	self accept: self objectToRunServiceOn = anObject! !

!AssessmentsServicesChecklist methodsFor: 'checks - services' stamp: 'SqR 3/8/2015 18:10'!
objectToRunServiceOn

	^42! !

!AssessmentsServicesChecklist class methodsFor: 'check services'!
checkServiceFor: anObject

	^self
		checklistWithCheckSelectorsNamed: self checkServiceSelectors
		toBePerformedWith: (Array with: anObject)! !

!AssessmentsServicesChecklist class methodsFor: 'check services'!
checkServiceSelectors

	^#(checkServicePass: checkServiceFail: checkServiceError:)! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 3/8/2015 18:10'!
bridgedValidator

	^SUnitVMValidatorTestCase withAllLookupSubclasses
		detect: [:one | one name = #TestObjectValidator]
		ifNone: [nil]! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 3/8/2015 18:10'!
checkValidationTestsWithFailure

	| testObject result validationFailure |
	self requiredBridgeCodeExists ifFalse: [^self].
	testObject := self newTestObject.
	result := self bridgedValidator assessment validationResultFor: testObject.
	self accept: result allChecksPassed not.
	self accept: result size = 1.
	self accept: result validationFailures size = 1.
	validationFailure := result validationFailures first.
	self accept: validationFailure prettyPrint = 'Required string is required'.
	self accept: validationFailure check checkSelector = #validateRequiredString! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 3/8/2015 18:10'!
checkValidationTestsWithoutFailure

	| testObject result |
	self requiredBridgeCodeExists ifFalse: [^self].
	testObject := self newTestObject.
	testObject requiredString: 'whatever'.
	result := self bridgedValidator assessment validationResultFor: testObject.
	self accept: result allChecksPassed.
	self accept: result size = 1.
	self accept: result passes size = 1! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 3/8/2015 18:10'!
newTestObject

	| testObjectClass |
	testObjectClass := self testObjectClass.
	testObjectClass isNil ifTrue: [^nil].
	^testObjectClass new! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 3/8/2015 18:10'!
requiredBridgeCodeExists

	^self bridgedValidator notNil
		and: [self newTestObject notNil]! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - bridged validation' stamp: 'SqR 8/1/2015 19:17'!
testObjectClass

	^AssessmentsClassResolver current selfTestsTestObject! !

!ValidationSelfVerificationChecklist methodsFor: 'checks - validation' stamp: 'SqR 3/8/2015 18:10'!
checkValidationValidator

	| result pass error validationFailure |
	result := self evaluateChecklist: ValidationValidator.
	self accept: result allChecksPassed not.
	self accept: result passes size = 2.
	self accept: result validationFailures size = 1.
	self accept: result errors size = 1.
	pass := result passes detect: [:one | one check checkSelector = #validatePass].
	self accept: pass prettyPrint = 'Result is acceptable'.
	pass := result passes detect: [:one | one check checkSelector = #validateSomeAspectPass].
	self accept: pass prettyPrint = 'Yourself is acceptable'.
	error := result errors first.
	self accept: error prettyPrint = ValidationValidator new errorText.
	self accept: error check checkSelector = #validateError.
	validationFailure := result validationFailures first.
	self accept: validationFailure prettyPrint = 'Yourself is required'.
	self accept: validationFailure check checkSelector = #validateSomeAspectFailure! !

!ValidationSelfVerificationChecklist methodsFor: 'checklist evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateChecklist: aChecklist

	^self evaluationContext
		validate: aChecklist new
		with: aChecklist assessment! !

!BenchmarksSelfVerificationChecklist methodsFor: 'checks - benchmarks' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkChecklist

	| result expectationFailure expectationMet |
	result := self evaluateChecklist: BenchmarkChecklist.
	self accept: result allChecksPassed not.
	self accept: result passes isEmpty.
	self accept: result benchmarkExpectationsFailed size = 1.
	expectationFailure := result benchmarkExpectationsFailed first.
	self accept: expectationFailure prettyPrint = 'Slow code'.
	self accept: expectationFailure prettyPrintForScenario = 'Unique'.
	self accept: expectationFailure iterationsPerSecond < expectationFailure expectedIterationsPerSecond.
	self accept: result benchmarkExpectationsMet size = 1.
	expectationMet := result benchmarkExpectationsMet first.
	self accept: expectationMet prettyPrint = 'Fast code'.
	self accept: expectationMet prettyPrintForScenario = 'Unique'.
	self accept: expectationMet iterationsPerSecond > expectationMet expectedIterationsPerSecond.
	self accept: result benchmarkPasses size = 3.
	self accept: (result benchmarkPasses anySatisfy: [:any | any check selector = #measureSomethingWithoutPrettyPrint]).
	self accept: (result benchmarkPasses anySatisfy: [:any | any check selector = #measureSomething]).
	self accept: (result benchmarkPasses anySatisfy: [:any | any check selector = #measureSomeExpectationToMeet]).
	self accept: (result benchmarkPasses allSatisfy: [:each | each prettyPrintForScenario = 'Unique']).
	self accept: result benchmarkResults size = 2.
	self accept: (result benchmarkResults anySatisfy: [:any | any check selector = #measureSomethingWithoutPrettyPrint]).
	self accept: (result benchmarkResults anySatisfy: [:any | any check selector = #measureSomething]).
	self accept: (result benchmarkResults anySatisfy: [:any | any prettyPrint = '2 + 3']).
	self accept: (result benchmarkResults anySatisfy: [:any | any prettyPrint = 'Measurement']).
	self accept: (result benchmarkResults allSatisfy: [:each | each prettyPrintForScenario = 'Unique'])! !

!Validator methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
adaptorValue

	^self value value! !

!Validator methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
aspectForReporting

	^self reportFailuresFor ifNil: [self aspect]! !

!Validator methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
value

	^self object perform: self aspect! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsDate

	self isDate: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsDefined

	self isDefined: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNonEmptyString

	self isNonEmptyString: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNonFutureDate

	self isNonFutureDate: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNonFutureTimestamp

	self isNonFutureTimestamp: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNonOldDate

	self isNonOldDate: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNonOldTimestamp

	self isNonOldTimestamp: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsNotZero

	self isNotZero: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsPositiveInteger

	self isPositiveInteger: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsTimestamp

	self isTimestamp: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsUndefined

	self isUndefined: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueIsValid

	self isValid: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValueNotEmpty

	self isNotEmpty: self adaptorValue! !

!Validator methodsFor: 'validation helpers - adaptor values' stamp: 'SqR 3/8/2015 18:10'!
adaptorValuesAreDefined

	self areDefined: self adaptorValue! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:27'!
areDefined: aValue

	aValue isNil
		ifTrue: [self failValidationBecause: self areRequired]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:27'!
isDate: aValue

	(aValue isKindOf: Date)
		ifFalse: [self failValidationBecause: self dateIsRequired]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:27'!
isDefined: aValue

	aValue isNil
		ifTrue: [self failValidationBecause: self isRequired]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:25'!
isNonEmptyString: aValue

	(aValue isString and: [aValue notEmpty])
		ifFalse: [self failValidationBecause: self isRequired]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:25'!
isNonFutureDate: aValue

	((aValue isKindOf: Date) and: [aValue <= Date today])
		ifFalse: [self failValidationBecause: self cannotBeInTheFuture]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:25'!
isNonFutureTimestamp: aValue

	((aValue isKindOf: self dateAndTimeClass) and: [aValue <= DateAndTime now])
		ifFalse: [self failValidationBecause: self cannotBeInTheFuture]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:26'!
isNonOldDate: aValue

	((aValue isKindOf: Date) and: [aValue >= self minimumDate])
		ifFalse: [self failValidationBecause: self cannotBeOldDate]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:26'!
isNonOldTimestamp: aValue

	((aValue isKindOf: self dateAndTimeClass) and: [aValue asDate >= self minimumDate])
		ifFalse: [self failValidationBecause: self cannotBeOldDate]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:26'!
isNotEmpty: aValue

	((aValue isKindOf: Collection) and: [aValue notEmpty])
		ifFalse: [self failValidationBecause: self isMissing]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:27'!
isNotZero: aValue

	aValue == 0
		ifTrue: [self failValidationBecause: self isZero]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:26'!
isPositiveInteger: aValue

	(aValue isInteger and: [aValue > 0])
		ifFalse: [self failValidationBecause: self mustBePositiveInteger]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:26'!
isTimestamp: aValue

	(aValue isKindOf: self dateAndTimeClass)
		ifFalse: [self failValidationBecause: self isRequired]! !

!Validator methodsFor: 'validation helpers' stamp: 'sqr 1/12/2016 19:27'!
isUndefined: aValue

	aValue notNil
		ifTrue: [self failValidationBecause: self shouldNotBeSpecified]! !

!Validator methodsFor: 'validation helpers' stamp: 'SqR 3/8/2015 18:10'!
isValid: aValue

	| results |
	results := self includeValidationResultsFor: aValue.
	results allChecksPassed ifFalse: [self failValidationBecause: self isInvalid]! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
areRequired

	^self prettyPrint, ' are required'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
cannotBeInTheFuture

	^self prettyPrint, ' cannot be in the future'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
cannotBeOldDate

	^self prettyPrint, ' cannot be prior to ', self minimumDate printString! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
dateIsRequired

	^self isRequired, '.  Use format MM/DD/YYYY'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
isInvalid

	^self prettyPrint, ' is invalid'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
isMissing

	^self prettyPrint isEmpty
		ifTrue: ['No selection has been made']
		ifFalse: ['No ', self prettyPrint, ' selection has been made']! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
isRequired

	^self prettyPrint, ' is required'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
isZero

	^self prettyPrint, ' must not be zero'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
mustBePositiveInteger

	^self prettyPrint, ' must be an integer greater than zero'! !

!Validator methodsFor: 'message patterns' stamp: 'SqR 3/8/2015 18:10'!
shouldNotBeSpecified

	^self prettyPrint, ' should not be specified'! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect

	^aspect! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect: anObject

	aspect := anObject! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
object

	^object! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
originalObject

	^originalObject! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
reportFailuresFor

	^reportFailuresFor! !

!Validator methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
reportFailuresFor: anObject

	reportFailuresFor := anObject! !

!Validator methodsFor: 'private - constants' stamp: 'sqr 1/12/2016 19:23'!
dateAndTimeClass

	^AssessmentsDateAndTime current dateAndTimeClass! !

!Validator methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
defaultPrettyPrint

	self aspect isNil ifTrue: [^super defaultPrettyPrint].
	^self aspect validationAspectPrettyPrint! !

!Validator methodsFor: 'private - constants' stamp: 'SqR 3/8/2015 18:10'!
minimumDate

	^Date
		newDay: 1
		monthNumber: 1
		year: 1901! !

!Validator methodsFor: 'evaluation - validation' stamp: 'SqR 2/22/2015 18:01'!
failValidationBecause: aString

	self validationFailureNotification new
		aspect: self reportFailuresFor;
		object: self object;
		signal: aString! !

!Validator methodsFor: 'validation helpers - result inclusion' stamp: 'SqR 3/8/2015 18:10'!
includeAdaptorValueValidationResults

	^self includeValidationResultsFor: self adaptorValue! !

!Validator methodsFor: 'validation helpers - result inclusion' stamp: 'SqR 3/8/2015 18:10'!
includeValidationResultsFor: anObject

	| results |
	self isDefined: anObject.
	results := anObject validate.
	self adoptAssessmentResult: results.
 	^results! !

!Validator methodsFor: 'validation helpers - result inclusion' stamp: 'SqR 3/8/2015 18:10'!
includeValueValidationResults

	^self includeValidationResultsFor: self value! !

!Validator methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
object: anObject

	object := anObject! !

!Validator methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
originalObject: anObject

	originalObject := anObject! !

!Validator methodsFor: 'private - notifications' stamp: 'SqR 3/8/2015 18:10'!
validationFailureNotification

	^CheckValidationFailureNotification! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsDate

	self isDate: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsDefined

	self isDefined: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNonEmptyString

	self isNonEmptyString: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNonFutureDate

	self isNonFutureDate: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNonFutureTimestamp

	self isNonFutureTimestamp: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNonOldDate

	self isNonOldDate: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNonOldTimestamp

	self isNonOldTimestamp: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsNotZero

	self isNotZero: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsPositiveInteger

	self isPositiveInteger: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsTimestamp

	self isTimestamp: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsUndefined

	self isUndefined: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueIsValid 

	self isValid: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valueNotEmpty

	self isNotEmpty: self value! !

!Validator methodsFor: 'validation helpers - values' stamp: 'SqR 3/8/2015 18:10'!
valuesAreDefined

	self areDefined: self value! !

!Validator class methodsFor: 'classification'!
abstractHierarchyRoot

	^Validator! !

!Validator class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!Validator class methodsFor: 'check lookup'!
checkSelectorPrefix

	^'validate'! !

!Validator class methodsFor: 'check services'!
checklistForClass: aClass

	self = self abstractHierarchyRoot ifTrue:
		[
			self = aClass ifTrue: [^aClass].
			^(aClass inheritsFrom: self)
				ifTrue: [aClass] ifFalse: [nil]
		].
	^super checklistForClass: aClass! !

!Validator class methodsFor: 'checks'!
newCheckWithCheckSelector: aSelector

	| answer |
	answer := super newCheckWithCheckSelector: aSelector.
	answer useAssessmentsValidationExecutionPolicy.
	^answer! !

!Validator class methodsFor: 'validation'!
validate: anObject

	^self assessment validationResultFor: anObject! !

!ValidationValidator methodsFor: 'validation - constants' stamp: 'SqR 3/8/2015 18:10'!
errorText

	^'Something blew up'! !

!ValidationValidator methodsFor: 'validation' stamp: 'SqR 3/8/2015 18:10'!
validateError

	self error: self errorText! !

!ValidationValidator methodsFor: 'validation' stamp: 'SqR 3/8/2015 18:10'!
validatePass

	self accept: 1 + 2 = 3! !

!ValidationValidator methodsFor: 'validation' stamp: 'SqR 3/8/2015 18:10'!
validateSomeAspectFailure

	self aspect: #yourself.
	self valueIsNonEmptyString! !

!ValidationValidator methodsFor: 'validation' stamp: 'SqR 3/8/2015 18:10'!
validateSomeAspectPass

	self aspect: #yourself.
	self valueIsDefined! !

!AbstractSUnitChecklistBridge class methodsFor: 'classification'!
abstractSUnitBridgeChecklistRoot

	^AbstractSUnitChecklistBridge! !

!AbstractSUnitChecklistBridge class methodsFor: 'classification'!
isAbstract
	"Abstract checklists exist for the sole purpose of refactoring,
	and are never used to perform checks.

	If it is necessary to mark sub hierarchies as abstract,
	then refine the implementation of abstractHierarchyRoot"

	^self = self sunitBridgeChecklistRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^self = self abstractSUnitBridgeChecklistRoot
		or: [self testCaseRootForVisibilityExists]! !

!AbstractSUnitChecklistBridge class methodsFor: 'classification'!
sunitBridgeChecklistRoot

	^self abstractSUnitBridgeChecklistRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'check lookup'!
checkSelectorLookupRoot

	^self sunitBridgeChecklistRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'check lookup'!
checkSelectorLookupSubclasses

	self shouldCreateMetaclasses ifFalse: [^super checkSelectorLookupSubclasses].
	self testCaseRootExists ifFalse: [^Array new].
	^Array with: self newMetaclassForTestCaseRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'check lookup'!
checkSelectorPrefix
	"Instance message selectors beginning with this
	prefix are interpreted to be check entry points"

	^'test'! !

!AbstractSUnitChecklistBridge class methodsFor: 'check services'!
checklistForClass: aClass

	(self = self sunitBridgeChecklistRoot
		and: [(aClass inheritsFrom: self testCaseRoot) not])
			ifTrue: [^nil].
	(self = self sunitBridgeChecklistRoot
		or: [self testCaseRootForVisibilityExists])
			ifTrue: [^super checklistForClass: aClass].
	^nil! !

!AbstractSUnitChecklistBridge class methodsFor: 'SUnit'!
metaclassClass

	self subclassResponsibility! !

!AbstractSUnitChecklistBridge class methodsFor: 'SUnit'!
newMetaclassForTestCaseRoot

	^self metaclassClass
		pretendToBe: self testCaseRoot
		subclassedFrom: self! !

!AbstractSUnitChecklistBridge class methodsFor: 'SUnit'!
testCaseRoot

	^nil! !

!AbstractSUnitChecklistBridge class methodsFor: 'SUnit'!
testCaseRootForVisibility

	^self testCaseRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'testing'!
shouldCreateMetaclasses

	^self ~= self abstractSUnitBridgeChecklistRoot! !

!AbstractSUnitChecklistBridge class methodsFor: 'testing'!
testCaseRootExists

	^self testCaseRoot notNil! !

!AbstractSUnitChecklistBridge class methodsFor: 'testing'!
testCaseRootForVisibilityExists

	^self testCaseRootForVisibility notNil! !

!SUnitTestCase class methodsFor: 'SUnit'!
metaclassClass

	^SUnitCheckRepositoryRootMetaclass! !

!SUnitTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 18:52'!
testCaseRoot

	^AssessmentsClassResolver current sunitTestCaseRoot! !

!SUnitTestCase class methodsFor: 'testing'!
shouldBeDisplayedInChecklistEvaluator

	^super shouldBeDisplayedInChecklistEvaluator
		and: [SUnitVMAbstractTestCase shouldBeDisplayedInChecklistEvaluator not]! !

!SUnitTestCase class methodsFor: 'classification'!
sunitBridgeChecklistRoot

	^SUnitTestCase! !

!SUnitTooTestCase class methodsFor: 'SUnit'!
metaclassClass

	^SUnitTooCheckRepositoryRootMetaclass! !

!SUnitTooTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 19:01'!
testCaseRoot

	^AssessmentsClassResolver current sunitTooTestCaseRoot! !

!SUnitTooTestCase class methodsFor: 'classification'!
sunitBridgeChecklistRoot

	^SUnitTooTestCase! !

!SUnitTooTestCase class methodsFor: 'prerequisites'!
usesClassBasedPrerequisites

	^false! !

!SUnitVMAbstractTestCase class methodsFor: 'testing'!
shouldCreateMetaclasses

	^self ~= self sunitBridgeChecklistRoot! !

!SUnitVMAbstractTestCase class methodsFor: 'classification'!
sunitBridgeChecklistRoot

	^SUnitVMAbstractTestCase! !

!SUnitVMAbstractTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 19:05'!
testCaseRootForVisibility

	^AssessmentsClassResolver current sunitVMTestCaseRootForVisibility! !

!SUnitVMBenchmarkTestCase class methodsFor: 'classification'!
metaclassClass

	^SUnitVMBenchmarkCheckRepositoryMetaclass! !

!SUnitVMBenchmarkTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 19:10'!
testCaseRoot

	^AssessmentsClassResolver current sunitVMBenchmarkTestCaseRoot! !

!SUnitVMTestCase class methodsFor: 'classification'!
metaclassClass

	^SUnitVMCheckRepositoryMetaclass! !

!SUnitVMTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 19:12'!
testCaseRoot

	^AssessmentsClassResolver sunitVMTestCaseRoot! !

!SUnitVMValidatorTestCase class methodsFor: 'classification'!
metaclassClass

	^SUnitVMValidatorCheckRepositoryMetaclass! !

!SUnitVMValidatorTestCase class methodsFor: 'classification'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!SUnitVMValidatorTestCase class methodsFor: 'SUnit' stamp: 'SqR 8/1/2015 19:15'!
testCaseRoot

	^AssessmentsClassResolver current sunitVMValidationTestCaseRoot! !

!CheckRepositoryMetaclass methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
atSelector: aSelector

	^self classToImpersonate methodDictionary
		at: aSelector
		ifAbsent: [nil]! !

!CheckRepositoryMetaclass methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:15'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self classForBrowsing value: nil! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
classToImpersonate

	^classToImpersonate! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
classToImpersonate: anObject

	classToImpersonate := anObject! !

!CheckRepositoryMetaclass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
subclasses: anObject

	subclasses := anObject! !

!CheckRepositoryMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
isAbstract

	^self classToImpersonate isAbstract! !

!CheckRepositoryMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks

	^self classToImpersonate shouldInheritChecks! !

!CheckRepositoryMetaclass methodsFor: 'selector lookup' stamp: 'SqR 3/8/2015 18:10'!
isKeywordSelector: aSelector

	^aSelector notEmpty and: [aSelector last = $:]! !

!CheckRepositoryMetaclass methodsFor: 'selector lookup' stamp: 'SqR 3/8/2015 18:10'!
isNotKeywordSelector: aSelector

	^(self isKeywordSelector: aSelector) not! !

!CheckRepositoryMetaclass methodsFor: 'selector lookup' stamp: 'SqR 3/8/2015 18:10'!
selector: aSelector beginsWith: aPrefix
	"This should be SequenceableCollection>>beginsWith:"

	aSelector size < aPrefix size ifTrue: [^false].
	1 to: aPrefix size do:
		[:eachIndex |
			(aSelector at: eachIndex) = (aPrefix at: eachIndex)
				ifFalse: [^false]
		].
	^true! !

!CheckRepositoryMetaclass methodsFor: 'comparing' stamp: 'SqR 3/8/2015 18:10'!
= anotherMetaclass

	^self class = anotherMetaclass class
		and: [self classToImpersonate = anotherMetaclass classToImpersonate]! !

!CheckRepositoryMetaclass methodsFor: 'comparing' stamp: 'SqR 3/8/2015 18:10'!
hash

	^self classToImpersonate hash! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
allCheckSelectors

	| answer pivotClass |
	answer := self checkSelectors.
	self shouldInheritChecks ifFalse: [^answer].
	pivotClass := self.
	[pivotClass isCheckSelectorLookupRoot] whileFalse:
		[
			pivotClass := pivotClass checkSelectorLookupSuperclass.
			pivotClass checkSelectorsDo: [:each | answer add: each]
		].
	^answer! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorDefinitionClass

	^self classToImpersonate! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorLookupRoot

	^self classToImpersonate checkSelectorLookupRoot! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorLookupSubclasses

	^self subclasses! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorLookupSuperclass

	^self superclass! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorPrefix

	^self classToImpersonate checkSelectorPrefix! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectors

	| answer |
	answer := Set new.
	self checkSelectorsDo: [:each | answer add: each].
	^answer! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorsDo: aBlock

	self selectorsDo:
		[:each |
			(self isCheckSelector: each)
				ifTrue: [aBlock value: each]
		]! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
isCheckSelector: aSelector

	^(self isNotKeywordSelector: aSelector)
		and: [self selector: aSelector beginsWith: self checkSelectorPrefix]! !

!CheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
isCheckSelectorLookupRoot

	^self = self checkSelectorLookupRoot! !

!CheckRepositoryMetaclass methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
assessment

	^self assessmentClass forChecklist: self! !

!CheckRepositoryMetaclass methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
evaluationResult

	^self assessment evaluationResult! !

!CheckRepositoryMetaclass methodsFor: 'assessments' stamp: 'SqR 3/8/2015 18:10'!
installContextForCheck: aCheck

	aCheck checklist: self! !

!CheckRepositoryMetaclass methodsFor: 'constants' stamp: 'SqR 3/8/2015 18:10'!
assessmentClass

	^self classToImpersonate assessmentClass! !

!CheckRepositoryMetaclass methodsFor: 'constants' stamp: 'SqR 3/8/2015 18:10'!
checkClass

	^self classToImpersonate checkClass! !

!CheckRepositoryMetaclass methodsFor: 'check services' stamp: 'SqR 3/8/2015 18:10'!
checklistForClass: aClass

	self classToImpersonate == aClass ifTrue: [^self].
	self checkSelectorLookupSubclasses do:
		[:one |
			| checklistForClass |
			checklistForClass := one checklistForClass: aClass.
			checklistForClass notNil ifTrue: [^checklistForClass]
		].
	^nil! !

!CheckRepositoryMetaclass methodsFor: 'check services' stamp: 'SqR 3/8/2015 18:10'!
checklistWithCheckSelectorsNamed: checkSelectors
toBePerformedWith: someArguments

	| answer |
	answer := self checklistWithCheckSelectorsNamed: checkSelectors.
	answer checksDo: [:each | each arguments: someArguments].
	^answer! !

!CheckRepositoryMetaclass methodsFor: 'custom checklists' stamp: 'SqR 3/8/2015 18:10'!
checklistWithCheckSelectorsNamed: checkSelectors

	| answer |
	answer := CustomChecklist new.
	checkSelectors do:
		[:each |
			| newCheck |
			newCheck := self newCheckWithCheckSelector: each.
			answer addCheck: newCheck
		].
	^answer! !

!CheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checks

	| answer |
	answer := OrderedCollection new.
	self checksDo: [:each | answer add: each].
	^answer! !

!CheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
checksDo: aBlock

	self checkSelectorsDo:
		[:each |
			| newCheck |
			newCheck := self newCheckWithCheckSelector: each.
			aBlock value: newCheck
		]! !

!CheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
newCheckWithCheckSelector: aSelector

	^self checkClass
		checklist: self
		checkSelector: aSelector! !

!CheckRepositoryMetaclass methodsFor: 'instance creation' stamp: 'SqR 3/8/2015 18:10'!
new

	^self classToImpersonate new! !

!CheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
newSubclasses

	^self classToImpersonate subclasses collect:
		[:each |
			self subclassClass
				pretendToBe: each
				subclassedFrom: self
		]! !

!CheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
pretendToBe: aClass
subclassedFrom: aSuperclass

	self classToImpersonate: aClass.
	self superclass: aSuperclass.
	self subclasses: self newSubclasses! !

!CheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
subclassClass

	^self class! !

!CheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self classToImpersonate prerequisites! !

!CheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
usesClassBasedPrerequisites

	^true! !

!CheckRepositoryMetaclass methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
shouldBeDisplayedInChecklistEvaluator

	^self classToImpersonate shouldBeDisplayedInChecklistEvaluator! !

!CheckRepositoryMetaclass methodsFor: 'lookup' stamp: 'SqR 3/8/2015 18:10'!
withAllLookupSubclasses

	| answer pivotIndex |
	answer := OrderedCollection with: self.
	pivotIndex := 1.
	[pivotIndex <= answer size] whileTrue:
		[
			| pivot |
			pivot := answer at: pivotIndex.
			answer addAll: pivot checkSelectorLookupSubclasses.
			pivotIndex := pivotIndex + 1
		].
	^answer! !

!CheckRepositoryMetaclass class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:33'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!CheckRepositoryMetaclass class methodsFor: 'instance creation'!
pretendToBe: aClass
subclassedFrom: aSuperclass

	^self new
		pretendToBe: aClass subclassedFrom: aSuperclass;
		yourself! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'constants' stamp: 'SqR 3/8/2015 18:10'!
assessmentClass

	^self impersonationRoot assessmentClass! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'constants' stamp: 'SqR 3/8/2015 18:10'!
checkClass

	^self impersonationRoot checkClass! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
basicPrerequisites

	^self classToImpersonate resources! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self prerequisitesForResources: self basicPrerequisites! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesForResources: aCollection

	^aCollection collect: [:each | self sunitPrerequisiteFor: each]! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
sunitPrerequisiteFor: aTestResource

	^SUnitPrerequisite for: aTestResource! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorLookupRoot

	^self impersonationRoot! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorPrefix

	^self impersonationRoot checkSelectorPrefix! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	self subclassResponsibility! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	self subclassResponsibility! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
newCheckWithCheckSelector: aSelector

	| answer |
	answer := super newCheckWithCheckSelector: aSelector.
	self installCheckExecutionPolicyOn: answer.
	^answer! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
shouldBeDisplayedInChecklistEvaluator

	^true! !

!AbstractSUnitCheckRepositoryMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks

	^self classToImpersonate shouldInheritSelectors! !

!SUnitCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	^SUnitTestCase! !

!SUnitCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	aCheck useSUnitExecutionPolicy! !

!SUnitCheckRepositoryRootMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks
	"In SUnit, TestCase fails when sent shouldInheritSelectors.
	Since the answer should be false, then make it so here"

	^false! !

!SUnitCheckRepositoryRootMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
subclassClass

	^SUnitCheckRepositoryMetaclass! !

!SUnitVMAbstractCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
checkSelectorPrefix

	^self classToImpersonate selectorPrefix! !

!SUnitVMAbstractCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	aCheck useSUnitVMExecutionPolicy! !

!SUnitVMBenchmarkCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
allScenarioSelectors

	^self classToImpersonate allSetUpSelectors! !

!SUnitVMBenchmarkCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	^SUnitVMBenchmarkTestCase! !

!SUnitVMBenchmarkCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	aCheck useSUnitVMBenchmarkExecutionPolicy! !

!SUnitVMCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	^SUnitVMTestCase! !

!SUnitVMValidatorCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	^SUnitVMValidatorTestCase! !

!SUnitVMValidatorCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	aCheck useSUnitVMValidationExecutionPolicy! !

!SUnitVMValidatorCheckRepositoryMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
isAbstract
	"AbstractValidator and all its subclasses are abstract by default.
	However, that does not make sense from the point of view of
	Assessments.  Therefore, pretend all validators are not abstract"

	^false! !

!SUnitVMValidatorCheckRepositoryMetaclass methodsFor: 'testing' stamp: 'SqR 3/8/2015 18:10'!
shouldBeDisplayedInChecklistEvaluator

	^false! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
basicPrerequisites
	"Dive through nested prerequisites"

	| toScan answer inclusionSet |
	toScan := super basicPrerequisites reverse.
	answer := OrderedCollection new.
	inclusionSet := Set new.
	[toScan isEmpty] whileFalse:
		[
			| next |
			next := toScan removeLast.
			(inclusionSet includes: next) ifFalse:
				[
					next resources reverseDo: [:each | toScan add: each].
					inclusionSet add: next.
					answer add: next
				]
		].
	^answer! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'prerequisites' stamp: 'SqR 3/8/2015 18:10'!
usesClassBasedPrerequisites

	^false! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
impersonationRoot

	^SUnitTooTestCase! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'checks' stamp: 'SqR 3/8/2015 18:10'!
installCheckExecutionPolicyOn: aCheck

	aCheck useSUnitTooExecutionPolicy! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
isCheckSelector: aSelector

	^(super isCheckSelector: aSelector) or:
		[
			| compiledMethod attributes |
			compiledMethod := self atSelector: aSelector.
			attributes := compiledMethod attributeMessages.
			attributes notNil and:
				[attributes anySatisfy: [:any | any selector = self pragmatizedSelectorTag]]
		]! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'check lookup' stamp: 'SqR 3/8/2015 18:10'!
pragmatizedSelectorTag

	^#test! !

!SUnitTooCheckRepositoryMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks
	"In SUnitToo, all test cases inherit selectors except the test case root"

	^true! !

!SUnitTooCheckRepositoryRootMetaclass methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
shouldInheritChecks
	"In SUnitToo, all test cases inherit selectors except the test case root"

	^false! !

!SUnitTooCheckRepositoryRootMetaclass methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
subclassClass

	^SUnitTooCheckRepositoryMetaclass! !

!AbstractResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printPrettyPrintDetailsOn: aStream

	aStream nextPutAll: ': '.
	aStream nextPutAll: self prettyPrint! !

!AbstractResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printResultDetailsOn: aStream

	^self! !

!AbstractResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
summaryPrintOn: aStream

	self printResultDetailsOn: aStream.
	self printPrettyPrintDetailsOn: aStream! !

!AbstractResult methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checkDo: aBlock

	^self! !

!AbstractResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^0! !

!AbstractResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint

	^prettyPrint ifNil: [String new]! !

!AbstractResult methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:26'!
classAndSelectorForBrowsingDo: aBlock

	^self! !

!AbstractResult methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
classificationTag

	^self class! !

!AbstractResult methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
displayClassificationTag

	^self class! !

!AbstractResult methodsFor: 'classification' stamp: 'SqR 3/6/2015 20:47'!
preventsAssessmentSuccess

	^self class preventsAssessmentSuccess! !

!AbstractResult methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self prettyPrint: String new! !

!AbstractResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrint: anObject

	prettyPrint := anObject! !

!AbstractResult class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:17'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!AbstractResult class methodsFor: 'classification'!
preventsAssessmentSuccess

	^false! !

!CheckResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
check

	^check! !

!CheckResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^self check checkExecutionCount! !

!CheckResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkSelector

	^self check checkSelector! !

!CheckResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checklist

	^self check checklist! !

!CheckResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
check: anObject

	check := anObject! !

!CheckResult methodsFor: 'enumeration' stamp: 'SqR 3/8/2015 18:10'!
checkDo: aBlock

	aBlock value: self check! !

!CheckResult methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:22'!
classAndSelectorForBrowsingDo: aBlock

	self check classAndSelectorForBrowsingDo: aBlock! !

!CheckResult methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
displayClassificationTag

	^self checklist! !

!CheckResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printResultDetailsOn: aStream

	super printResultDetailsOn: aStream.
	self check printBrieflyOn: aStream! !

!CheckResult class methodsFor: 'instance creation'!
for: aCheck

	^self new
		check: aCheck;
		yourself! !

!CheckBenchmarkPass methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario

	^prettyPrintForScenario ifNil: ['<no scenario>']! !

!CheckBenchmarkPass methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario: anObject

	prettyPrintForScenario := anObject! !

!CheckBenchmarkPass methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printPrettyPrintDetailsOn: aStream

	aStream
		nextPutAll: '@';
		nextPutAll: self prettyPrintForScenario.
	super printPrettyPrintDetailsOn: aStream! !

!CheckBenchmarkResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^0! !

!CheckBenchmarkResult methodsFor: 'accessing' stamp: 'SqR 2/22/2015 19:26'!
iterationsPerSecond

	^iterationsPerSecond ifNil: [0.0]! !

!CheckBenchmarkResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario

	^prettyPrintForScenario! !

!CheckBenchmarkResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
iterationsPerSecond: anObject

	iterationsPerSecond := anObject! !

!CheckBenchmarkResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prettyPrintForScenario: anObject

	prettyPrintForScenario := anObject! !

!CheckBenchmarkResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
iterationsPerSecondPrintString

	^self printStringForDouble: self iterationsPerSecond! !

!CheckBenchmarkResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printPrettyPrintDetailsOn: aStream

	aStream
		nextPutAll: '@';
		nextPutAll: self prettyPrintForScenario;
		nextPutAll: ': '.
	self prettyPrint notEmpty ifTrue:
		[
			aStream
				nextPutAll: self prettyPrint;
				nextPutAll: ' ran at '.
		].
	aStream
		nextPutAll: self iterationsPerSecondPrintString;
		nextPutAll: ' iterations / second'! !

!CheckBenchmarkResult methodsFor: 'printing - doubles' stamp: 'sqr 1/14/2016 01:23'!
printStringForDouble: aNumber

	^AssessmentsNumberPrinter current printStringForDouble: aNumber! !

!CheckBenchmarkResult methodsFor: 'printing - doubles' stamp: 'sqr 1/14/2016 01:27'!
usesNormalPrintPolicyFor: aNumber

	^AssessmentsNumberPrinter current usesNormalPrintPolicyFor: aNumber! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
actualSpeed

	^self iterationsPerSecond / self expectedIterationsPerSecond! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'accessing' stamp: 'SqR 2/22/2015 19:26'!
expectedIterationsPerSecond

	^expectedIterationsPerSecond ifNil: [1.0]! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing' stamp: 'SqR 2/22/2015 19:26'!
actualSpeedPrintString

	| actualSpeed |
	actualSpeed := self actualSpeed.
	^actualSpeed < 1.0
		ifTrue: [self suffixForSlowerBy: actualSpeed reciprocal]
		ifFalse: [self suffixForFasterBy: actualSpeed]! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printPrettyPrintDetailsOn: aStream

	super printPrettyPrintDetailsOn: aStream.
	aStream
		nextPutAll: ', ';
		nextPutAll: self actualSpeedPrintString! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing' stamp: 'SqR 2/22/2015 19:26'!
shortActualSpeedPrintString

	| actualSpeed |
	actualSpeed := self actualSpeed.
	^actualSpeed < 1.0
		ifTrue: [self shortSuffixForSlowerBy: actualSpeed reciprocal]
		ifFalse: [self shortSuffixForFasterBy: actualSpeed]! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
expectedIterationsPerSecond: anObject

	expectedIterationsPerSecond := anObject! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing - suffixes' stamp: 'SqR 3/8/2015 18:10'!
shortSuffixForFasterBy: aDouble

	^self suffixFor: 'faster' by: aDouble suffixedBy: String new! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing - suffixes' stamp: 'SqR 3/8/2015 18:10'!
shortSuffixForSlowerBy: aDouble

	^self suffixFor: 'slower' by: aDouble suffixedBy: String new! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing - suffixes' stamp: 'sqr 1/14/2016 01:27'!
suffixFor: description by: aDouble suffixedBy: aSuffix

	| doublePrintString usesNormalPolicy multiplierString |
	doublePrintString := self printStringForDouble: aDouble.
	usesNormalPolicy := self usesNormalPrintPolicyFor: aDouble.
	multiplierString := usesNormalPolicy ifTrue: ['x'] ifFalse: [' times'].
	^doublePrintString, multiplierString, ' ', description, aSuffix! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing - suffixes' stamp: 'SqR 3/8/2015 18:10'!
suffixForFasterBy: aDouble

	^self suffixFor: 'faster' by: aDouble suffixedBy: ' than expected'! !

!CheckBenchmarkAbstractExpectationNotice methodsFor: 'printing - suffixes' stamp: 'SqR 3/8/2015 18:10'!
suffixForSlowerBy: aDouble

	^self suffixFor: 'slower' by: aDouble suffixedBy: ' than expected'! !

!CheckBenchmarkExpectationFailure methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^1! !

!CheckBenchmarkExpectationFailure class methodsFor: 'classification'!
preventsAssessmentSuccess

	^true! !

!CheckError class methodsFor: 'classification'!
preventsAssessmentSuccess

	^true! !

!CheckFailure class methodsFor: 'classification'!
preventsAssessmentSuccess

	^true! !

!CheckValidationFailure methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect

	^aspect! !

!CheckValidationFailure methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
object

	^object! !

!CheckValidationFailure methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
aspect: anObject

	aspect := anObject! !

!CheckValidationFailure methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
object: anObject

	object := anObject! !

!CheckNotice methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^0! !

!CheckNotice methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
notice

	^notice! !

!CheckNotice methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
notice: anObject

	notice := anObject! !

!CheckNotice methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printPrettyPrintDetailsOn: aStream

	aStream nextPutAll: ': '.
	aStream nextPutAll: self notice! !

!PrerequisiteResult methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:22'!
classAndSelectorForBrowsingDo: aBlock

	self prerequisite classAndSelectorForBrowsingDo: aBlock! !

!PrerequisiteResult methodsFor: 'classification' stamp: 'SqR 3/8/2015 18:10'!
displayClassificationTag

	^self prerequisite! !

!PrerequisiteResult methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisite

	^prerequisite! !

!PrerequisiteResult methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisite: anObject

	prerequisite := anObject! !

!PrerequisiteResult methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printResultDetailsOn: aStream

	super printResultDetailsOn: aStream.
	aStream print: self prerequisite! !

!PrerequisiteResult class methodsFor: 'instance creation'!
for: aPrerequisite

	^self new
		prerequisite: aPrerequisite;
		yourself! !

!PrerequisiteResult class methodsFor: 'classification'!
preventsAssessmentSuccess

	^true! !

!AssessmentsPortabilityService methodsFor: 'private - compiling' stamp: 'sqr 1/14/2016 21:54'!
compile: aString in: aClass
	"Generally, these blocks should not reference their context.
	Currently only used for VisualWorks"

	"self isCuis ifTrue: [^self cuisCompile: aString in: aClass]."
	self isVisualWorks ifTrue: [^self visualWorksCompile: aString in: aClass].
	self unknownDialect! !

!AssessmentsPortabilityService methodsFor: 'private - compiling' stamp: 'SqR 8/1/2015 18:37'!
cuisCompile: aString in: aClass

	| methodAndNode |
	methodAndNode := aClass
		basicCompile: aString
		notifying: nil
		trailer: self defaultMethodTrailer
		ifFail: [AssessmentMalfunctionNotification signal].
	^methodAndNode method! !

!AssessmentsPortabilityService methodsFor: 'private - compiling' stamp: 'SqR 8/1/2015 18:37'!
visualWorksCompile: aString in: aClass

	| methodNode |
	methodNode := aClass compilerClass new
		compile: aString
		in: aClass
		notifying: nil
		ifFail: [AssessmentMalfunctionNotification signal].
	^methodNode generate! !

!AssessmentsPortabilityService methodsFor: 'private - dialect testing' stamp: 'SqR 3/24/2015 19:46'!
isCuis

	^[
		| versionCuisIndex |
		versionCuisIndex := Smalltalk version indexOfSubCollection: 'Cuis' startingAt: 1.
		versionCuisIndex > 0
	]
		on: Error
		do: [:ex | ex return: false] ! !

!AssessmentsPortabilityService methodsFor: 'private - dialect testing' stamp: 'sqr 1/12/2016 18:48'!
isVSE

	^[Smalltalk respondsTo: #recursiveMessage]
		on: Error do: [:ex | ex return: false]! !

!AssessmentsPortabilityService methodsFor: 'private - dialect testing' stamp: 'SqR 3/24/2015 19:46'!
isVisualWorks

	^[Smalltalk includesKey: #ObjectMemory]
		on: Error
		do: [:ex | ex return: false]! !

!AssessmentsPortabilityService methodsFor: 'private - class resolution' stamp: 'SqR 8/1/2015 19:23'!
resolveCuisClassNamed: aSymbol

	^Smalltalk at: aSymbol ifAbsent: [nil]! !

!AssessmentsPortabilityService methodsFor: 'private - class resolution' stamp: 'sqr 1/12/2016 18:48'!
resolveVSEClassNamed: aSymbol

	^Smalltalk at: aSymbol ifAbsent: [nil]! !

!AssessmentsPortabilityService methodsFor: 'private - class resolution' stamp: 'sqr 1/16/2016 21:37'!
resolveVisualWorksClassNamed: aString
	"aString may be a symbol, or a qualified reference path"

	| resolver block |
	aString isSymbol ifTrue: [^Smalltalk at: aString].
	resolver := '
	[
		[{', aString, '} value]
			on: Error
			do: [:ex | ex return]
		]
	]'.
	block := self compile: resolver in: self.
	^block value! !

!AssessmentsPortabilityService methodsFor: 'private' stamp: 'SqR 8/1/2015 18:41'!
unknownDialect

	AssessmentMalfunctionNotification signal: 'Unknown dialect'! !

!AssessmentsPortabilityService class methodsFor: 'accessing' stamp: 'SqR 3/24/2015 19:47'!
current
	"It is not possible to distribute Assessments with all its portability code because (sigh)
	such code would be unloadable across dialects.  No problem, let's compile as needed
	--- but without making code dirty"

	current isNil ifTrue: [self current: self new].
	^current! !

!AssessmentsPortabilityService class methodsFor: 'accessing' stamp: 'SqR 3/24/2015 19:45'!
release

	self current: nil! !

!AssessmentsPortabilityService class methodsFor: 'accessing' stamp: 'SqR 3/24/2015 19:46'!
releaseAll

	self withAllSubclasses do: [:each | each release]! !

!AssessmentsPortabilityService class methodsFor: 'private - accessing' stamp: 'SqR 3/24/2015 19:26'!
current: anInstance

	current := anInstance! !

!AssessmentsPortabilityService class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:32'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew! !

!AssessmentsClassResolver methodsFor: 'private - accessing - self tests' stamp: 'sqr 1/12/2016 18:50'!
newSelfTestsTestObject

	self isCuis ifTrue: [^self resolveCuisClassNamed: #TestObject].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'TestObject'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #TestObject].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - self tests' stamp: 'SqR 8/1/2015 19:17'!
selfTestsTestObject: aClass

	selfTestsTestObject := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'sqr 1/12/2016 18:51'!
newSunitFailureNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #TestFailure].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.TestFailure'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #TestFailure].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'sqr 1/12/2016 18:51'!
newSunitResumableFailureNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #ResumableTestFailure].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.ResumableTestFailure'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #ResumableTestFailure].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'sqr 1/12/2016 18:51'!
newSunitTestCaseRoot

	self isCuis ifTrue: [^self resolveCuisClassNamed: #TestCase].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.TestCase'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #TestCase].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'sqr 1/12/2016 18:51'!
newSunitTestSkipNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #TestSkip].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.TestSkip'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #TestSkip].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'SqR 8/1/2015 18:50'!
sunitFailureNotification: aClass

	sunitFailureNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'SqR 8/1/2015 18:53'!
sunitResumableFailureNotification: aClass

	sunitResumableFailureNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'SqR 8/1/2015 18:51'!
sunitTestCaseRoot: aClass

	sunitTestCaseRoot := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnit' stamp: 'SqR 8/1/2015 18:57'!
sunitTestSkipNotification: aClass

	sunitTestSkipNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitToo' stamp: 'sqr 1/12/2016 18:52'!
newSunitTooFailureNotification

	self isCuis ifTrue: [^nil].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'SUnit.FailedAssertion'].
	self isVSE ifTrue: [^nil].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitToo' stamp: 'sqr 1/12/2016 18:52'!
newSunitTooTestCaseRoot

	self isCuis ifTrue: [^nil].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'SUnit.TestCase'].
	self isVSE ifTrue: [^nil].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitToo' stamp: 'SqR 8/1/2015 18:59'!
sunitTooFailureNotification: aClass

	sunitTooFailureNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitToo' stamp: 'SqR 8/1/2015 19:00'!
sunitTooTestCaseRoot: aClass

	sunitTooTestCaseRoot := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:52'!
newSunitVMBenchmarkExpectationNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #BenchmarkExpectationNotification].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'BenchmarkExpectationNotification'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #BenchmarkExpectationNotification].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:53'!
newSunitVMBenchmarkMeasurementNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #BenchmarkMeasurementNotification].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'BenchmarkMeasurementNotification'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #BenchmarkMeasurementNotification].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:53'!
newSunitVMBenchmarkTestCaseRoot

	self isCuis ifTrue: [^self resolveCuisClassNamed: #AbstractBenchmarkTestCase].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.AbstractBenchmarkTestCase'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #AbstractBenchmarkTestCase].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:53'!
newSunitVMNoticeNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #TestNoticeNotification].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'TestNoticeNotification'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #TestNoticeNotification].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:53'!
newSunitVMTestCaseRootForVisibility

	self isCuis ifTrue: [^self resolveCuisClassNamed: #AbstractTestCase].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.AbstractTestCase'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #AbstractTestCase].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:54'!
newSunitVMValidationFailureNotification

	self isCuis ifTrue: [^self resolveCuisClassNamed: #ValidationFailureException].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'ValidationFailureException'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #ValidationFailureException].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'sqr 1/12/2016 18:54'!
newSunitVMValidationTestCaseRoot

	self isCuis ifTrue: [^self resolveCuisClassNamed: #AbstractValidator].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'XProgramming.SUnit.AbstractValidator'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #AbstractValidator].
	self unknownDialect! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:06'!
sunitVMBenchmarkExpectationNotification: aClass

	sunitVMBenchmarkExpectationNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:08'!
sunitVMBenchmarkMeasurementNotification: aClass

	sunitVMBenchmarkMeasurementNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:09'!
sunitVMBenchmarkTestCaseRoot: aClass

	sunitVMBenchmarkTestCaseRoot := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:10'!
sunitVMNoticeNotification: aClass

	sunitVMNoticeNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:03'!
sunitVMTestCaseRootForVisibility: aClass

	sunitVMTestCaseRootForVisibility := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:13'!
sunitVMValidationFailureNotification: aClass

	sunitVMValidationFailureNotification := aClass! !

!AssessmentsClassResolver methodsFor: 'private - accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:14'!
sunitVMValidationTestCaseRoot: aClass

	sunitVMValidationTestCaseRoot := aClass! !

!AssessmentsClassResolver methodsFor: 'accessing - self tests' stamp: 'SqR 8/1/2015 19:17'!
selfTestsTestObject

	selfTestsTestObject notNil ifTrue: [^selfTestsTestObject].
	self selfTestsTestObject: self newSelfTestsTestObject.
	^selfTestsTestObject! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnit' stamp: 'SqR 8/1/2015 18:52'!
sunitFailureNotification

	sunitFailureNotification notNil ifTrue: [^sunitFailureNotification].
	self sunitFailureNotification: self newSunitFailureNotification.
	^sunitFailureNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnit' stamp: 'SqR 8/1/2015 18:52'!
sunitResumableFailureNotification

	sunitResumableFailureNotification notNil ifTrue: [^sunitResumableFailureNotification].
	self sunitResumableFailureNotification: self newSunitResumableFailureNotification.
	^sunitResumableFailureNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnit' stamp: 'SqR 8/1/2015 18:50'!
sunitTestCaseRoot

	sunitTestCaseRoot notNil ifTrue: [^sunitTestCaseRoot].
	self sunitTestCaseRoot: self newSunitTestCaseRoot.
	^sunitTestCaseRoot! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnit' stamp: 'SqR 8/1/2015 18:56'!
sunitTestSkipNotification

	sunitTestSkipNotification notNil ifTrue: [^sunitTestSkipNotification].
	self sunitTestSkipNotification: self newSunitTestSkipNotification.
	^sunitTestSkipNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitToo' stamp: 'SqR 8/1/2015 18:58'!
sunitTooFailureNotification

	sunitTooFailureNotification notNil ifTrue: [^sunitTooFailureNotification].
	self sunitTooFailureNotification: self newSunitTooFailureNotification.
	^sunitTooFailureNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitToo' stamp: 'SqR 8/1/2015 19:00'!
sunitTooTestCaseRoot

	sunitTooTestCaseRoot notNil ifTrue: [^sunitTooTestCaseRoot].
	self sunitTooTestCaseRoot: self newSunitTooTestCaseRoot.
	^sunitTooTestCaseRoot! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:06'!
sunitVMBenchmarkExpectationNotification

	sunitVMBenchmarkExpectationNotification notNil ifTrue: [^sunitVMBenchmarkExpectationNotification].
	self sunitVMBenchmarkExpectationNotification: self newSunitVMBenchmarkExpectationNotification.
	^sunitVMBenchmarkExpectationNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:08'!
sunitVMBenchmarkMeasurementNotification

	sunitVMBenchmarkMeasurementNotification notNil ifTrue: [^sunitVMBenchmarkMeasurementNotification].
	self sunitVMBenchmarkMeasurementNotification: self newSunitVMBenchmarkMeasurementNotification.
	^sunitVMBenchmarkMeasurementNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:09'!
sunitVMBenchmarkTestCaseRoot

	sunitVMBenchmarkTestCaseRoot notNil ifTrue: [^sunitVMBenchmarkTestCaseRoot].
	self sunitVMBenchmarkTestCaseRoot: self newSunitVMBenchmarkTestCaseRoot.
	^sunitVMBenchmarkTestCaseRoot! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:11'!
sunitVMNoticeNotification

	sunitVMNoticeNotification notNil ifTrue: [^sunitVMNoticeNotification].
	self sunitVMNoticeNotification: self newSunitVMNoticeNotification.
	^sunitVMNoticeNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:12'!
sunitVMTestCaseRoot

	^self sunitTestCaseRoot! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:04'!
sunitVMTestCaseRootForVisibility

	sunitVMTestCaseRootForVisibility notNil ifTrue: [^sunitVMTestCaseRootForVisibility].
	self sunitVMTestCaseRootForVisibility: self newSunitVMTestCaseRootForVisibility.
	^sunitVMTestCaseRootForVisibility! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:13'!
sunitVMValidationFailureNotification

	sunitVMValidationFailureNotification notNil ifTrue: [^sunitVMValidationFailureNotification].
	self sunitVMValidationFailureNotification: self newSunitVMValidationFailureNotification.
	^sunitVMValidationFailureNotification! !

!AssessmentsClassResolver methodsFor: 'accessing - SUnitVM' stamp: 'SqR 8/1/2015 19:15'!
sunitVMValidationTestCaseRoot

	sunitVMValidationTestCaseRoot notNil ifTrue: [^sunitVMValidationTestCaseRoot].
	self sunitVMValidationTestCaseRoot: self newSunitVMValidationTestCaseRoot.
	^sunitVMValidationTestCaseRoot! !

!AssessmentsDateAndTime methodsFor: 'accessing' stamp: 'sqr 1/12/2016 19:19'!
dateAndTimeClass

	dateAndTimeClass isNil ifFalse: [^dateAndTimeClass].
	self dateAndTimeClass: self newDateAndTimeClass.
	^dateAndTimeClass! !

!AssessmentsDateAndTime methodsFor: 'accessing' stamp: 'sqr 1/13/2016 21:37'!
nowLocal
	"VSE has no notion of timestamp time zones"

	self isVSE ifTrue: [^self nowUTC].
	^self nowUTC asLocal! !

!AssessmentsDateAndTime methodsFor: 'accessing' stamp: 'sqr 1/12/2016 19:34'!
nowLocalPrintString

	^self printStringFor: self nowLocal! !

!AssessmentsDateAndTime methodsFor: 'accessing' stamp: 'sqr 1/12/2016 19:21'!
nowUTC

	^self dateAndTimeClass now! !

!AssessmentsDateAndTime methodsFor: 'accessing' stamp: 'sqr 1/12/2016 19:34'!
nowUTCPrintString

	^self printStringFor: self nowUTC! !

!AssessmentsDateAndTime methodsFor: 'private - accessing' stamp: 'sqr 1/12/2016 19:19'!
dateAndTimeClass: anObject

	dateAndTimeClass := anObject! !

!AssessmentsDateAndTime methodsFor: 'private - accessing' stamp: 'sqr 1/14/2016 21:59'!
newDateAndTimeClass

	self isCuis ifTrue: [^self resolveCuisClassNamed: #DateAndTime].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'DateAndTime'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #DateTime].
	self unknownDialect! !

!AssessmentsDateAndTime methodsFor: 'private - printing' stamp: 'sqr 1/12/2016 19:36'!
printStringFor: aDateAndTime

	| stream answer |
	stream := WriteStream on: (String new: 32).
	self isCuis ifTrue: [aDateAndTime printWithMsOn: stream].
	self isVisualWorks ifTrue: [aDateAndTime printOn: stream].
	self isVSE ifTrue: [aDateAndTime printOn: stream].
	answer := stream contents.
	answer isEmpty ifTrue: [self unknownDialect].
	^answer! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'jmv 5/31/2016 11:09'!
appendFileStreamFor: aFilenameString

	self isCuis ifTrue: [^(self cuisCurrentDirectory // aFilenameString) appendStream].
	self isVisualWorks ifTrue: [^aFilenameString asFilename appendStream].
	self isVSE ifTrue:
		[
			| writeFileStream |
			writeFileStream := self writeFileStreamFor: aFilenameString.
			^[writeFileStream setToEnd; yourself] ifCurtailed: [writeFileStream close]
		].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'sqr 1/14/2016 22:21'!
defaultDirectory

	self isCuis ifTrue: [^self cuisCurrentDirectory].
	self isVisualWorks ifTrue: [^self visualWorksCurrentDirectory].
	self isVSE ifTrue: [^self vseCurrentDirectory].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'sqr 1/14/2016 22:12'!
deleteFileNamed: aString

	self isCuis ifTrue: [^self cuisDeleteFileNamed: aString].
	self isVisualWorks ifTrue: [^self visualWorksDeleteFileNamed: aString].
	self isVSE ifTrue: [^self vseDeleteFileNamed: aString].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'sqr 1/14/2016 22:12'!
fileNameExists: aString

	self isCuis ifTrue: [^self cuisFileNameExists: aString].
	self isVisualWorks ifTrue: [^self visualWorksFileNameExists: aString].
	self isVSE ifTrue: [^self vseFileNameExists: aString].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'sqr 1/14/2016 22:13'!
sizeForFileName: aString

	self isCuis ifTrue: [^self cuisSizeForFileName: aString].
	self isVisualWorks ifTrue: [^self visualWorksSizeForFileName: aString].
	self isVSE ifTrue: [^self vseSizeForFileName: aString].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'file system' stamp: 'jmv 5/31/2016 11:02'!
writeFileStreamFor: aFilenameString

	self isCuis ifTrue: [^(self cuisCurrentDirectory // aFilenameString) writeStream].
	self isVisualWorks ifTrue: [^aFilenameString asFilename writeStream].
	self isVSE ifTrue: [^aFilenameString asFileSystemPath asFile writeStream].	
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'pb 5/25/2016 01:07'!
cuisCurrentDirectory

	| directoryEntry |
	directoryEntry := self resolveCuisClassNamed: #DirectoryEntry.
	^directoryEntry currentDirectory! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'pb 5/25/2016 01:17'!
cuisDeleteFileNamed: aString

	| fileEntry |
	fileEntry := self cuisCurrentDirectory // aString.
	fileEntry delete! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'SqR 11/18/2015 15:18'!
cuisFileNameExists: aString

	^self cuisCurrentDirectory includesKey: aString! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'pb 5/25/2016 01:18'!
cuisSizeForFileName: aString

	| fileEntry |
	fileEntry := self cuisCurrentDirectory // aString.
	^fileEntry fileSize! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'pb 5/25/2016 01:07'!
currentDirectoryReceiver

	self isCuis ifTrue: [^self resolveCuisClassNamed: #DirectoryEntry].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'FileDirectory'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #Directory].
	self unknownDialect! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:00'!
visualWorksCurrentDirectory
	
	| fileDirectory |
	fileDirectory := self resolveVisualWorksClassNamed: 'FileDirectory'.
	^fileDirectory default! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'SqR 11/18/2015 15:27'!
visualWorksDeleteFileNamed: aString

	self visualWorksCurrentDirectory deleteFileNamed: aString! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:18'!
visualWorksFileNameExists: aString

	| entry |
	entry := self visualWorksCurrentDirectory directoryEntryFor: aString.
	^entry exists! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:18'!
visualWorksSizeForFileName: aString

	| entry |
	entry := self visualWorksCurrentDirectory directoryEntryFor: aString.
	^entry fileSize! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:21'!
vseCurrentDirectory

	| fileDirectory |
	fileDirectory := self resolveVSEClassNamed: #Directory.
	^fileDirectory current! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:28'!
vseDeleteFileNamed: aString 

	| entry |
	entry := self vseCurrentDirectory existingFileNamed: aString.
	entry remove
! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:24'!
vseFileNameExists: aString

	| entry |
	entry := self vseCurrentDirectory existingFileNamed: aString.
	^entry notNil! !

!AssessmentsFileDirectory methodsFor: 'private - file system' stamp: 'sqr 1/14/2016 22:26'!
vseSizeForFileName: aString

	^self vseCurrentDirectory fileSizeForFileNamed: aString
! !

!AssessmentsGC methodsFor: 'gc' stamp: 'sqr 1/16/2016 21:16'!
flushNativeCodeCache

	self garbageCollect! !

!AssessmentsGC methodsFor: 'gc' stamp: 'sqr 1/16/2016 21:16'!
flushNewSpace

	self isCuis ifTrue: [^self].
	self isVisualWorks ifTrue: [^self visualWorksFlushNewSpace].
	self isVSE ifTrue: [^self].
	self unknownDialect! !

!AssessmentsGC methodsFor: 'gc' stamp: 'sqr 1/16/2016 21:17'!
garbageCollect

	self isCuis ifTrue: [^Smalltalk garbageCollect].
	self isVisualWorks ifTrue:
		[
			| objectMemory |
			objectMemory := self resolveVisualWorksClassNamed: 'ObjectMemory'.
			^objectMemory garbageCollect
		].
	self isVSE ifTrue: [^Smalltalk unusedMemory].
	self unknownDialect! !

!AssessmentsGC methodsFor: 'private - gc' stamp: 'sqr 1/16/2016 21:18'!
visualWorksFlushNewSpace

	<primitive: 322>
	self error: 'New space flush failed'! !

!AssessmentsNumberPrinter methodsFor: 'printing' stamp: 'sqr 1/25/2016 16:11'!
printStringForDouble: aDoublePrecisionFloat

	self isCuis ifTrue: [^aDoublePrecisionFloat asFloat printString].
	self isVisualWorks ifTrue: [^self visualWorksPrintStringForDouble: aDoublePrecisionFloat].
	self isVSE ifTrue: [^aDoublePrecisionFloat asFloat printString].
	self unknownDialect! !

!AssessmentsNumberPrinter methodsFor: 'printing' stamp: 'sqr 1/14/2016 22:06'!
usesNormalPrintPolicyFor: aNumber

	self isCuis ifTrue: [^true].
	self isVisualWorks ifTrue:
		[
			| printPolicy |
			printPolicy := self visualWorksPrintPolicyFor: aNumber.
			^printPolicy = self visualWorksNormalPrintPolicy
		].
	self isVSE ifTrue: [^true].
	self unknownDialect! !

!AssessmentsNumberPrinter methodsFor: 'private - printing' stamp: 'sqr 1/14/2016 22:06'!
visualWorksNormalPrintPolicy

	^'#,#.000'! !

!AssessmentsNumberPrinter methodsFor: 'private - printing' stamp: 'sqr 1/14/2016 22:06'!
visualWorksPrintPolicyFor: aNumber

	aNumber < 0.1 ifTrue: [^self visualWorksScientificPrintPolicy].
	aNumber >= 10000 ifTrue: [^self visualWorksScientificPrintPolicy].
	^self visualWorksNormalPrintPolicy! !

!AssessmentsNumberPrinter methodsFor: 'private - printing' stamp: 'sqr 1/14/2016 01:23'!
visualWorksPrintStringForDouble: aDouble

	| printPolicyClass writeBuffer printPolicy |
	printPolicyClass := self resolveVisualWorksClassNamed: 'NumberPrintPolicy'.
	writeBuffer := String new writeStream.
	printPolicy := self printPolicyFor: aDouble.
	printPolicyClass
		print: aDouble asDouble
		on: writeBuffer
		using: printPolicy.
	^writeBuffer contents! !

!AssessmentsNumberPrinter methodsFor: 'private - printing' stamp: 'sqr 1/14/2016 22:06'!
visualWorksScientificPrintPolicy

	^'#.000E0'! !

!AssessmentsProcessScheduler methodsFor: 'private - scheduling' stamp: 'sqr 1/19/2016 03:38'!
basicStep: aProcess lookingFor: targetCompiledMethod	

	| suspendedContext |
	suspendedContext := aProcess suspendedContext.
	suspendedContext := self basicStepContext: suspendedContext lookingFor: targetCompiledMethod.
	suspendedContext isNil ifTrue: [^nil].
	aProcess suspendedContext: suspendedContext.
	^suspendedContext! !

!AssessmentsProcessScheduler methodsFor: 'private - scheduling' stamp: 'sqr 1/19/2016 03:36'!
basicStepContext: aSuspendedContext lookingFor: targetCompiledMethod
	"Do not do this forever, otherwise the image may hang"

	^self
		basicStepContext: aSuspendedContext
		noMoreThan: self maximumStepsWhenDebugging
		timesLookingFor: targetCompiledMethod! !

!AssessmentsProcessScheduler methodsFor: 'private - scheduling' stamp: 'sqr 1/19/2016 03:39'!
basicStepContext: aSuspendedContext
noMoreThan: anInteger
timesLookingFor: targetCompiledMethod

	| newSuspendedContext count |
	count := 0.
	newSuspendedContext := aSuspendedContext.
	[count > anInteger] whileFalse:
		[
			newSuspendedContext method == targetCompiledMethod ifTrue: [^newSuspendedContext].
			count := count + 1.
			newSuspendedContext := newSuspendedContext step
		].
	^nil! !

!AssessmentsProcessScheduler methodsFor: 'private - scheduling' stamp: 'sqr 1/19/2016 03:46'!
maximumStepsWhenDebugging
	"Try looking for the compiled method for the check selector
	by stepping a limited number of times.  Too much stepping
	can cause the image to hang if for some reason (like a bug)
	the compiled method cannot be found.  When this constant
	was initially chosen, the actual number of steps was 28"

	^100! !

!AssessmentsProcessScheduler methodsFor: 'scheduling' stamp: 'sqr 1/26/2016 09:32'!
newProcessFor: aBlock
	"VSE seems to have no notion of creating a process in suspended state"

	self isCuis ifTrue: [^aBlock newProcess].
	self isVisualWorks ifTrue: [^aBlock newProcess].
	self isVSE ifTrue: [^[aBlock halt; value] fork].
	self unknownDialect! !

!AssessmentsProcessScheduler methodsFor: 'scheduling' stamp: 'sqr 1/19/2016 03:20'!
openDebuggerOn: aProcess
	"This is setup so in VSE the process merely resumes --- opening the debugger
	on a process that is not the active process is also cumbersome"

	self isCuis ifTrue: [^Debugger openInterrupt: 'Debug It' onProcess: aProcess].
	self isVisualWorks ifTrue: [^Debugger openInterrupt: 'Debug It' onProcess: aProcess].
	self isVSE ifTrue: [^self].
	self unknownDialect! !

!AssessmentsProcessScheduler methodsFor: 'scheduling' stamp: 'sqr 1/19/2016 03:18'!
step: aProcess lookingFor: targetCompiledMethod
	"VSE has no straightforward way to step a process forward"

	self isCuis ifTrue: [^self basicStep: aProcess lookingFor: targetCompiledMethod].
	self isVisualWorks ifTrue: [^self basicStep: aProcess lookingFor: targetCompiledMethod].
	self isVSE ifTrue: [^self].
	self unknownDialect! !

!AssessmentsProfiler methodsFor: 'private - profiling' stamp: 'sqr 1/25/2016 15:39'!
newProfiler

	self isCuis ifTrue: [^self resolveCuisClassNamed: #MessageTally].
	self isVisualWorks ifTrue: [^self resolveVisualWorksClassNamed: 'TimeProfiler'].
	self isVSE ifTrue: [^self resolveVSEClassNamed: #Profiler].
	self unknownDialect! !

!AssessmentsProfiler methodsFor: 'private - profiling' stamp: 'SqR 8/1/2015 19:22'!
profiler

	^profiler! !

!AssessmentsProfiler methodsFor: 'private - profiling' stamp: 'SqR 8/1/2015 19:20'!
profiler: aClass

	profiler := aClass! !

!AssessmentsProfiler methodsFor: 'profiling' stamp: 'sqr 1/25/2016 15:40'!
profile: aBlock

	self profiler isNil ifTrue: [self profiler: self newProfiler].
	self profiler isNil ifTrue: [self error: 'Profiler not found'].
	self isCuis ifTrue: [^self profiler spyOn: aBlock].
	self isVisualWorks ifTrue: [^self profiler profile: aBlock].
	self isVSE ifTrue: [^aBlock profile].
	self unknownDialect! !

!AssessmentsProfiler methodsFor: 'testing' stamp: 'sqr 1/22/2016 20:55'!
shouldProfileEvaluation

	self isCuis ifTrue:
		[
			| eventSensor |
			eventSensor := self resolveCuisClassNamed: #Sensor.
			^eventSensor shiftPressed
		].
	self isVisualWorks ifTrue:
		[
			
			| inputState |
			inputState := self resolveVisualWorksClassNamed: 'InputState'.
			^inputState default shiftDown
		].
	self isVSE ifTrue:
		[
			| notifier |
			notifier := self resolveVSEClassNamed: #Notifier.  "a global, actually"
			^notifier isShiftDown
		].
	self unknownDialect! !

!AssessmentsSystemOrganization methodsFor: 'organization' stamp: 'sqr 1/16/2016 21:48'!
classes

	self isCuis ifTrue: [^self cuisClasses].
	self isVisualWorks ifTrue: [^Array new].
	self isVSE ifTrue: [^Array new].
	self unknownDialect! !

!AssessmentsSystemOrganization methodsFor: 'private - organization' stamp: 'sqr 1/16/2016 21:45'!
cuisCategories

	^self cuisSystemOrganization categoriesMatching: 'Assessments-*'! !

!AssessmentsSystemOrganization methodsFor: 'private - organization' stamp: 'sqr 1/16/2016 21:44'!
cuisSystemOrganization

	^self resolveCuisClassNamed: #SystemOrganization! !

!AssessmentsSystemOrganization methodsFor: 'organization - cuis' stamp: 'sqr 1/16/2016 21:45'!
cuisClassNames

	^self cuisCategories
		inject: OrderedCollection new
		into:
			[:answer :each |
				answer
					addAll: (self cuisSystemOrganization listAtCategoryNamed: each);
					yourself
			]! !

!AssessmentsSystemOrganization methodsFor: 'organization - cuis' stamp: 'sqr 1/16/2016 21:46'!
cuisClasses

	^self cuisClassNames collect: [:each | self resolveCuisClassNamed: each]! !

!AssessmentsSystemOrganization methodsFor: 'testing' stamp: 'sqr 1/16/2016 21:46'!
isSupported

	^self isCuis! !

!CheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck: aCheck inTheContextOf: anEvaluationContext

	| newActualReceiver |
	newActualReceiver := self
		newActualReceiverForCheck: aCheck
		inTheContextOf: anEvaluationContext.
	aCheck actualReceiver: newActualReceiver.
	self aboutToExecuteOn: newActualReceiver! !

!CheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck: aCheck inTheContextOf: anEvaluationContext

	self doneExecutingOn: aCheck actualReceiver.
	aCheck actualReceiver: nil! !

!CheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'sqr 1/19/2016 03:46'!
evaluate: aCheck inTheContextOf: anEvaluationContext

	^[
		| prerequisitesAvailable |
		prerequisitesAvailable := aCheck ensurePrerequisitesInTheContextOf: anEvaluationContext.
		prerequisitesAvailable
			ifTrue: [aCheck basicEvaluateInTheContextOf: anEvaluationContext]
			ifFalse: [self reportSkippedCheck: aCheck inTheContextOf: anEvaluationContext]
	]
		on: self exceptionsToTrap
		do:
			[:ex |
				self
					handleOccurrenceOf: ex
					whileEvaluating: aCheck
					inTheContextOf: anEvaluationContext
			]! !

!CheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/6/2015 19:59'!
fastPassEvaluate: aCheck inTheContextOf: anEvaluationContext

	^[
		| prerequisitesAvailable |
		prerequisitesAvailable := aCheck ensurePrerequisitesInTheContextOf: anEvaluationContext.
		prerequisitesAvailable
			ifTrue: [aCheck basicFastPassEvaluateInTheContextOf: anEvaluationContext]
			ifFalse: [self reportSkippedCheck: aCheck inTheContextOf: anEvaluationContext]
	]
		on: self exceptionsToTrap
		do:
			[:ex |
				self
					handleOccurrenceOf: ex
					whileEvaluating: aCheck
					inTheContextOf: anEvaluationContext
			]! !

!CheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
raisePassNotificationFor: aCheck

	^self! !

!CheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
aboutToExecuteOn: anActualReceiver

	^self! !

!CheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneExecutingOn: anActualReceiver

	^self! !

!CheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
newActualReceiverForCheck: aCheck inTheContextOf: anEvaluationContext

	^aCheck checklist new! !

!CheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 2/22/2015 18:01'!
reportSkippedCheck: aCheck inTheContextOf: anEvaluationContext

	| notification prerequisitesDescription |
	prerequisitesDescription := String new writeStream.
	aCheck prerequisites
		do: [:each | each printOn: prerequisitesDescription]
		separatedBy: [prerequisitesDescription nextPutAll: ', '].
	notification := self checkSkipNotification new.
	notification signal: 'Missing prerequisites: ', prerequisitesDescription contents! !

!CheckExecutionPolicy methodsFor: 'private - perform debug' stamp: 'sqr 1/19/2016 03:44'!
basicDebug: aCheck

	| newProcess ratchet |
	ratchet := Semaphore new.
	newProcess :=
		[
			self
				newSuspendedProcessToDebugEvaluationOf: aCheck
				signalling: ratchet
		]
			on: self exceptionsToTrap
			do: [:ex | ex resignalAsAssessmentMalfunctionNotification].
	newProcess isNil ifTrue: [^self].
	AssessmentsProcessScheduler current openDebuggerOn: newProcess.
	ratchet wait! !

!CheckExecutionPolicy methodsFor: 'private - perform debug' stamp: 'SqR 3/8/2015 18:10'!
compiledMethodForEvaluationOf: aCheck

	| pivotClass |
	pivotClass := aCheck receiver.
	[
		| compiledMethod |
		compiledMethod := pivotClass atSelector: aCheck checkSelector.
		compiledMethod notNil ifTrue: [^compiledMethod].
		pivotClass := pivotClass checkSelectorLookupSuperclass.
		pivotClass notNil
	] whileTrue.
	self notifyMalfunction: 'Could not find check selector to debug'.
	^nil! !

!CheckExecutionPolicy methodsFor: 'private - perform debug' stamp: 'sqr 1/19/2016 03:14'!
newProcessToDebugEvaluationOf: aCheck signalling: aSemaphore

	^AssessmentsProcessScheduler current newProcessFor:
		[
			[self unprotectedBasicPerform: aCheck]
				ensure: [aSemaphore signal]
		]! !

!CheckExecutionPolicy methodsFor: 'private - perform debug' stamp: 'sqr 1/19/2016 03:39'!
newSuspendedProcessToDebugEvaluationOf: aCheck
signalling: aSemaphore

	| targetCompiledMethod newProcess suspendedContext |
	targetCompiledMethod := self compiledMethodForEvaluationOf: aCheck.
	targetCompiledMethod isNil ifTrue: [^nil].
	newProcess := self newProcessToDebugEvaluationOf: aCheck signalling: aSemaphore.
	suspendedContext := AssessmentsProcessScheduler current
		step: newProcess lookingFor: targetCompiledMethod.
	suspendedContext isNil
		ifTrue: [self notifyMalfunction: 'Could not find target compiled method while trying to debug'].
	^newProcess! !

!CheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 2/22/2015 19:15'!
basicPerform: aCheck

	| checkPassed |
	checkPassed := true.
	[self unprotectedBasicPerform: aCheck]
		on: self resumableFailureNotifications
		do:
			[:ex |
				checkPassed := false.
				ex pass
			].
	^checkPassed! !

!CheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
handleOccurrenceOf: anException
whileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	aCheck handle: anException inTheContextOf: anEvaluationContext! !

!CheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
unprotectedBasicPerform: aCheck

	^aCheck actualReceiver
		perform: aCheck checkSelector
		withArguments: aCheck arguments! !

!CheckExecutionPolicy methodsFor: 'private - perform run to failure' stamp: 'SqR 11/18/2015 15:59'!
basicRunToFailure: aCheck

	| ratchet checkPassed |
	checkPassed := true.
	ratchet := Semaphore new.
	[
		[
			[
				[self unprotectedBasicPerform: aCheck]
					ifCurtailed: [checkPassed := false]
			]
				on: self exceptionsToTrap
				do:
					[:ex |
						checkPassed := checkPassed
							and: [ex preventsCheckSuccess not].
						ex pass
					]
		]
			ensure: [ratchet signal]
	] fork.
	ratchet wait.
	^checkPassed! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationFailedNotification

	^CheckBenchmarkExpectationFailedNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
benchmarkExpectationMetNotification

	^CheckBenchmarkExpectationMetNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
benchmarkMeasurementNotification

	^CheckBenchmarkMeasurementNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
checkSkipNotification

	^CheckSkipNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
resumableFailureNotification

	^CheckResumableFailureNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
resumableFailureNotifications

	^self resumableFailureNotification! !

!CheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
validationFailureNotification

	^CheckValidationFailureNotification! !

!CheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'sqr 1/19/2016 03:44'!
debugPerformCheck: aCheck inTheContextOf: anEvaluationContext

	| checkPassed |
	checkPassed := true.
	[self basicDebug: aCheck]
		on: self resumableFailureNotifications
		do:
			[:ex |
				checkPassed := false.
				ex pass
			].
	checkPassed ifFalse: [^self].
	self raisePassNotificationFor: aCheck! !

!CheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheck: aCheck inTheContextOf: anEvaluationContext

	| checkPassed |
	checkPassed := self basicPerform: aCheck.
	checkPassed ifFalse: [^nil].
	^self fastPerformCheckPassResultFor: aCheck! !

!CheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheckPassResultFor: aCheck

	^String new! !

!CheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
performCheck: aCheck inTheContextOf: anEvaluationContext

	| checkPassed |
	checkPassed := self basicPerform: aCheck.
	checkPassed ifFalse: [^self].
	self raisePassNotificationFor: aCheck! !

!CheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
runToFailurePerformCheck: aCheck inTheContextOf: anEvaluationContext

	| checkPassed |
	checkPassed := self basicRunToFailure: aCheck.
	checkPassed ifTrue: [self raisePassNotificationFor: aCheck]! !

!CheckExecutionPolicy methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
exceptionsToTrap

	^exceptionsToTrap! !

!CheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
exceptionsToTrap: anObject

	exceptionsToTrap := anObject! !

!CheckExecutionPolicy methodsFor: 'private - exceptions to trap' stamp: 'SqR 10/6/2015 13:46'!
firstExceptionsToTrap

	^OrderedCollection new
		add: Error;
		yourself! !

!CheckExecutionPolicy methodsFor: 'private - exceptions to trap' stamp: 'SqR 10/6/2015 14:00'!
newExceptionsToTrap
	"Do not use fold: to make this easier to port to other Smalltalks"

	| exceptions answer |
"	exceptions := GenericException subclasses asSet.
	exceptions remove: ControlInterrupt.
	exceptions remove: UnhandledException.
	exceptions remove: TerminateException."
	exceptions := self firstExceptionsToTrap.
	Exception subclasses do:
		[:each |
			(exceptions includes: each)
				ifFalse: [exceptions add: each]
		].
	answer := exceptions first.
	2 to: exceptions size do: [:each | answer := answer, (exceptions at: each)].
	^answer! !

!CheckExecutionPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self exceptionsToTrap: self newExceptionsToTrap! !

!CheckExecutionPolicy methodsFor: 'private - errors' stamp: 'SqR 3/8/2015 18:10'!
malfunctionNotification

	^AssessmentMalfunctionNotification! !

!CheckExecutionPolicy methodsFor: 'private - errors' stamp: 'SqR 2/22/2015 18:01'!
notifyMalfunction: aString

	| newNotification |
	newNotification := self malfunctionNotification new.
	newNotification signal: aString! !

!CheckExecutionPolicy methodsFor: 'perform - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
performAttemptToStartPrerequisite: aPrerequisiteStartAttempt

	aPrerequisiteStartAttempt receiver start.
	^true! !

!CheckExecutionPolicy methodsFor: 'perform - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
performAttemptToStopPrerequisite: aPrerequisiteStartAttempt

	aPrerequisiteStartAttempt receiver stop.
	^true! !

!CheckExecutionPolicy methodsFor: 'perform - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesFor: aCheck

	^aCheck checklist prerequisites! !

!CheckExecutionPolicy class methodsFor: 'private - accessing'!
current

	^current! !

!CheckExecutionPolicy class methodsFor: 'private - accessing'!
current: anObject

	current := anObject! !

!CheckExecutionPolicy class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:18'!
new
	"Become independent of whether super new sends initialize"

	self current notNil ifTrue: [^self current].
	self current: self basicNew initialize.
	^self current! !

!CheckExecutionPolicy class methodsFor: 'initialize-release' stamp: 'sqr 1/14/2016 23:05'!
releaseAll

	self withAllSubclasses do: [:each | each current: nil]! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
aboutToExecuteOn: anActualReceiver

	anActualReceiver setUp! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneExecutingOn: anActualReceiver

	anActualReceiver tearDown! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
newActualReceiverForCheck: aCheck inTheContextOf: anEvaluationContext

	| answer |
	answer := super
		newActualReceiverForCheck: aCheck
		inTheContextOf: anEvaluationContext.
	answer setTestSelector: aCheck checkSelector.
	^answer! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
copyValuesFromAbstractChecklist

	| newChecklist |
	newChecklist := AbstractChecklist new.
	self passNotification: newChecklist passNotification.
	self isAcceptable: newChecklist isAcceptable.
	self isUnacceptable: newChecklist isUnacceptable! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	super initialize.
	self copyValuesFromAbstractChecklist! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
failureNotification

	^CheckFailureNotification! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheckPassResultFor: aCheck

	^self isAcceptable! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 2/22/2015 20:04'!
raisePassNotificationFor: aCheck

	self passNotification signal: self isAcceptable! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleKnownSUnitException: anException

	^self! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleSUnitFailureNotification: anException

	| newException |
	newException := self failureNotification new.
	newException messageText: anException messageText.
	anException resignalAs: newException! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleSUnitTestSkipNotification: anException

	| newException |
	newException := self skipNotification new.
	newException messageText: anException messageText.
	anException resignalAs: newException! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
handleOccurrenceOf: anException
whileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	self handleKnownSUnitException: anException.
	super
		handleOccurrenceOf: anException
		whileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
isAcceptable

	^isAcceptable! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
isAcceptable: anObject

	isAcceptable := anObject! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
isUnacceptable

	^isUnacceptable! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
isUnacceptable: anObject

	isUnacceptable := anObject! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
passNotification

	^passNotification! !

!AbstractSUnitCheckExecutionPolicy methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
passNotification: anObject

	passNotification := anObject! !

!SUnitCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 8/1/2015 18:55'!
handleKnownSUnitException: anException
	"No polymorphism because direct reference to exception classes is not possible.
	Some SUnit variants do not provide test skip notifications"

	super handleKnownSUnitException: anException.
	(anException isKindOf: self sunitResumableFailureNotification)
		ifTrue: [self handleSUnitResumableFailureNotification: anException].
	(anException isKindOf: self sunitFailureNotification)
		ifTrue: [self handleSUnitFailureNotification: anException].
	self sunitTestSkipNotification notNil ifTrue:
		[
			(anException isKindOf: self sunitTestSkipNotification)
				ifTrue: [self handleSUnitTestSkipNotification: anException]
		]! !

!SUnitCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 2/22/2015 18:03'!
handleSUnitResumableFailureNotification: anException

	| newException |
	newException := self resumableFailureNotification new.
	newException messageText: anException messageText.
	anException resignalAs: newException! !

!SUnitCheckExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 18:51'!
sunitFailureNotification

	^AssessmentsClassResolver current sunitFailureNotification! !

!SUnitCheckExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 18:53'!
sunitResumableFailureNotification

	^AssessmentsClassResolver current sunitResumableFailureNotification! !

!SUnitCheckExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 18:57'!
sunitTestSkipNotification

	^AssessmentsClassResolver current sunitTestSkipNotification! !

!SUnitVMCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleKnownSUnitException: anException
	"No polymorphism because direct reference to exception classes is not possible"

	super handleKnownSUnitException: anException.
	(anException isKindOf: self sunitVMNoticeNotification)
		ifTrue: [self handleSUnitVMNoticeNotification: anException]! !

!SUnitVMCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 2/22/2015 18:04'!
handleSUnitVMNoticeNotification: anException

	| newException |
	newException := self noticeNotification new.
	newException messageText: anException messageText.
	newException notice: anException notice text.
	anException resignalAs: newException! !

!SUnitVMCheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
noticeNotification

	^CheckNoticeNotification! !

!SUnitVMCheckExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 19:11'!
sunitVMNoticeNotification

	^AssessmentsClassResolver current sunitVMNoticeNotification! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck: aCheck inTheContextOf: anEvaluationContext
	"The scenario selector iteration sends setUp*"

	^self! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck: aCheck inTheContextOf: anEvaluationContext
	"The scenario selector iteration sends tearDown"

	^self! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
allScenarioSelectorsFor: aCheck

	^aCheck checklist allScenarioSelectors! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
allScenariosPerform: aBlock
withCheck: aCheck
inTheContextOf: anEvaluationContext

	(self allScenarioSelectorsFor: aCheck) do:
		[:eachScenarioSelector |
			| newActualReceiver |
			newActualReceiver := self
				newActualReceiverForCheck: aCheck
				inTheContextOf: anEvaluationContext.
			aCheck actualReceiver: newActualReceiver.
			newActualReceiver selectorForSetUp: eachScenarioSelector.
			newActualReceiver perform: eachScenarioSelector.
			aBlock value: aCheck.
			super doneEvaluatingCheck: aCheck inTheContextOf: anEvaluationContext
		]! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
debugPerformCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					debugPerformCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheck: aCheck inTheContextOf: anEvaluationContext
	"For the sake of simplicity, benchmark evaluation does not support fast pass execution"

	self performCheck: aCheck inTheContextOf: anEvaluationContext.
	^nil! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
performCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					performCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
runToFailurePerformCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					runToFailurePerformCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneExecutingOn: anActualReceiver
	"If there are no scenarios, then anActualReceiver is never set"

	anActualReceiver notNil ifTrue: [super doneExecutingOn: anActualReceiver]! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleKnownSUnitException: anException
	"No polymorphism because direct reference to exception classes is not possible"

	super handleKnownSUnitException: anException.
	(anException isKindOf: self sunitVMBenchmarkExpectationNotification)
		ifTrue: [self handleSUnitVMBenchmarkExpectationNotification: anException].
	(anException isKindOf: self sunitVMBenchmarkMeasurementNotification)
		ifTrue: [self handleSUnitVMBenchmarkMeasurementNotification: anException]! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 2/22/2015 18:04'!
handleSUnitVMBenchmarkExpectationNotification: anException

	| measurement newException |
	measurement := anException benchmarkMeasurement.
	newException := measurement isMet
		ifTrue: [self benchmarkExpectationMetNotification new]
		ifFalse: [self benchmarkExpectationFailedNotification new].
	newException messageText: measurement prettyPrintForMeasurement.
	newException prettyPrintForScenario: measurement prettyPrintForSetUp.
	newException iterationsPerSecond: measurement iterationsPerSecond.
	newException expectedIterationsPerSecond: measurement expectedIterationsPerSecond.
	anException resignalAs: newException! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 2/22/2015 18:04'!
handleSUnitVMBenchmarkMeasurementNotification: anException

	| measurement newException |
	measurement := anException benchmarkMeasurement.
	newException := self benchmarkMeasurementNotification new.
	newException messageText: measurement prettyPrintForMeasurement.
	newException prettyPrintForScenario: measurement prettyPrintForSetUp.
	newException iterationsPerSecond: measurement iterationsPerSecond.
	anException resignalAs: newException! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
resumableFailureNotifications

	^super resumableFailureNotifications, CheckBenchmarkExpectationFailedNotification! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 19:07'!
sunitVMBenchmarkExpectationNotification

	^AssessmentsClassResolver current sunitVMBenchmarkExpectationNotification! !

!SUnitVMBenchmarkExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 19:08'!
sunitVMBenchmarkMeasurementNotification

	^AssessmentsClassResolver current sunitVMBenchmarkMeasurementNotification! !

!SUnitVMValidationExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleKnownSUnitException: anException
	"No polymorphism because direct reference to exception classes is not possible"

	super handleKnownSUnitException: anException.
	(anException isKindOf: self sunitVMValidationFailureNotification)
		ifTrue: [self handleSUnitVMValidationFailureNotification: anException]! !

!SUnitVMValidationExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleSUnitVMValidationFailureNotification: anException

	| validationFailure newException |
	validationFailure := anException validationFailure.
	newException := self validationFailureNotification new.
	newException messageText: validationFailure description.
	newException object: validationFailure object.
	newException aspect: validationFailure aspect.
	anException resignalAs: newException! !

!SUnitVMValidationExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
newActualReceiverForCheck: aCheck inTheContextOf: anEvaluationContext

	| answer |
	answer := super
		newActualReceiverForCheck: aCheck
		inTheContextOf: anEvaluationContext.
	answer setTestSelector: aCheck checkSelector.
	answer object: anEvaluationContext objectToValidate.
	answer originalObject: anEvaluationContext originalObjectToValidate.
	^answer! !

!SUnitVMValidationExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 19:13'!
sunitVMValidationFailureNotification

	^AssessmentsClassResolver current sunitVMValidationFailureNotification! !

!SUnitTooCheckExecutionPolicy methodsFor: 'private - SUnit exception handling' stamp: 'SqR 3/8/2015 18:10'!
handleKnownSUnitException: anException
	"No polymorphism because direct reference to exception classes is not possible"

	super handleKnownSUnitException: anException.
	(anException isKindOf: self sunitTooFailureNotification)
		ifTrue: [self handleSUnitFailureNotification: anException]! !

!SUnitTooCheckExecutionPolicy methodsFor: 'perform - prerequisites' stamp: 'SqR 3/8/2015 18:10'!
prerequisitesFor: aCheck
	"SUnitToo uses instance-based prerequisites"

	| bogusActualReceiver |
	bogusActualReceiver := self
		newActualReceiverForCheck: aCheck
		inTheContextOf: nil.
	^aCheck receiver prerequisitesForResources: bogusActualReceiver resources! !

!SUnitTooCheckExecutionPolicy methodsFor: 'private - SUnit exceptions' stamp: 'SqR 8/1/2015 19:01'!
sunitTooFailureNotification

	^AssessmentsClassResolver current sunitTooFailureNotification! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheck: aCheck inTheContextOf: anEvaluationContext

	^self! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 2/22/2015 18:02'!
raisePassNotificationFor: aCheck

	aCheck actualReceiver checkBenchmarkPassNotification new
		prettyPrintForScenario: aCheck actualReceiver prettyPrintForScenario;
		signal: aCheck actualReceiver isAcceptable! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
allScenarioSelectorsFor: aCheck

	^aCheck checklist allScenarioSelectors! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'private - perform' stamp: 'SqR 3/8/2015 18:10'!
allScenariosPerform: aBlock
withCheck: aCheck
inTheContextOf: anEvaluationContext

	(self allScenarioSelectorsFor: aCheck) do:
		[:eachScenarioSelector |
			| newActualReceiver defaultPrettyPrintForScenario |
			defaultPrettyPrintForScenario := aCheck checklist defaultPrettyPrintForScenario: eachScenarioSelector.
			newActualReceiver := self
				newActualReceiverForCheck: aCheck
				inTheContextOf: anEvaluationContext.
			aCheck actualReceiver: newActualReceiver.
			newActualReceiver defaultPrettyPrintForScenario: defaultPrettyPrintForScenario.
			self aboutToExecuteOn: newActualReceiver.
			newActualReceiver perform: eachScenarioSelector.
			aBlock value: aCheck.
			super doneEvaluatingCheck: aCheck inTheContextOf: anEvaluationContext
		]! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
debugPerformCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					debugPerformCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheck: aCheck inTheContextOf: anEvaluationContext
	"For the sake of simplicity, benchmark evaluation does not support fast pass execution"

	self performCheck: aCheck inTheContextOf: anEvaluationContext.
	^nil! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
performCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					performCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'perform - checks' stamp: 'SqR 3/8/2015 18:10'!
runToFailurePerformCheck: aCheck inTheContextOf: anEvaluationContext

	self
		allScenariosPerform:
			[:check |
				super
					runToFailurePerformCheck: check
					inTheContextOf: anEvaluationContext
			]
		withCheck: aCheck
		inTheContextOf: anEvaluationContext! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheck: aCheck inTheContextOf: anEvaluationContext
	"The scenario selector iteration takes care of this"

	^self! !

!AssessmentsBenchmarkCheckExecutionPolicy methodsFor: 'private - exceptions' stamp: 'SqR 3/8/2015 18:10'!
resumableFailureNotifications

	^super resumableFailureNotifications, CheckBenchmarkExpectationFailedNotification! !

!AssessmentsCheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
aboutToExecuteOn: anActualReceiver

	anActualReceiver aboutToEvaluateCheck! !

!AssessmentsCheckExecutionPolicy methodsFor: 'private - execution - checks' stamp: 'SqR 3/8/2015 18:10'!
doneExecutingOn: anActualReceiver

	anActualReceiver doneEvaluatingCheck! !

!AssessmentsCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
fastPerformCheckPassResultFor: aCheck

	^aCheck actualReceiver isAcceptable! !

!AssessmentsCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 2/22/2015 20:04'!
raisePassNotificationFor: aCheck

	aCheck actualReceiver passNotification
		signal: aCheck actualReceiver isAcceptable! !

!AssessmentsCheckExecutionPolicy methodsFor: 'private - exceptions to trap' stamp: 'SqR 10/6/2015 13:46'!
firstExceptionsToTrap

	^OrderedCollection new
		add: AssessmentNotification;
		addAll: super firstExceptionsToTrap;
		yourself! !

!AssessmentsValidationCheckExecutionPolicy methodsFor: 'execution' stamp: 'SqR 3/8/2015 18:10'!
newActualReceiverForCheck: aCheck inTheContextOf: anEvaluationContext

	| answer |
	answer := super newActualReceiverForCheck: aCheck inTheContextOf: anEvaluationContext.
	answer object: anEvaluationContext objectToValidate.
	answer originalObject: anEvaluationContext originalObjectToValidate.
	^answer! !

!ProtectedMessageSend methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
printBrieflyOn: aStream

	aStream nextPutAll: (self selector ifNil: ['[none]'])! !

!ProtectedMessageSend methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
receiverPrintString

	^self receiver printString! !

!ProtectedMessageSend methodsFor: 'comparison' stamp: 'SqR 3/8/2015 18:10'!
= aMessageSend

	^self selector == aMessageSend selector
		and: [self receiver = aMessageSend receiver]! !

!ProtectedMessageSend methodsFor: 'comparison' stamp: 'sqr 1/25/2016 16:00'!
hash

	^self selector hash bitXor: self receiver hash! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
actualReceiver

	^self receiver! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
arguments: anObject

	arguments := anObject! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
executionPolicy

	^executionPolicy! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
executionPolicy: anObject

	executionPolicy := anObject! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
receiver

	^receiver! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
receiver: anObject

	receiver := anObject! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
selector

	^selector! !

!ProtectedMessageSend methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
selector: anObject

	selector := anObject! !

!ProtectedMessageSend methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
arguments

	^arguments! !

!ProtectedMessageSend methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^0! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
assessmentsBenchmarkExecutionPolicyClass

	^AssessmentsBenchmarkCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
assessmentsExecutionPolicyClass

	^AssessmentsCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
assessmentsValidationExecutionPolicyClass

	^AssessmentsValidationCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
sunitExecutionPolicyClass

	^SUnitCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
sunitTooExecutionPolicyClass

	^SUnitTooCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
sunitVMBenchmarkExecutionPolicyClass

	^SUnitVMBenchmarkExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
sunitVMExecutionPolicyClass

	^SUnitVMCheckExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy classes' stamp: 'SqR 3/8/2015 18:10'!
sunitVMValidationExecutionPolicyClass

	^SUnitVMValidationExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	self subclassResponsibility! !

!ProtectedMessageSend methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
ensurePrerequisitesInTheContextOf: anEvaluationContext

	^true! !

!ProtectedMessageSend methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
evaluateInTheContextOf: anEvaluationContext

	^self executionPolicy evaluate: self inTheContextOf: anEvaluationContext! !

!ProtectedMessageSend methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
handle: anException inTheContextOf: anEvaluationContext

	anException
		occurredWhileEvaluating: self
		inTheContextOf: anEvaluationContext! !

!ProtectedMessageSend methodsFor: 'browsing' stamp: 'SqR 9/7/2015 15:24'!
classAndSelectorForBrowsingDo: aBlock

	aBlock value: self receiver classForBrowsing value: self selector! !

!ProtectedMessageSend methodsFor: 'private' stamp: 'SqR 3/8/2015 18:10'!
initialize

	self arguments: #().
	self useAssessmentsExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newAssessmentsBenchmarkExecutionPolicy

	^self assessmentsBenchmarkExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newAssessmentsExecutionPolicy

	^self assessmentsExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newAssessmentsValidationExecutionPolicy

	^self assessmentsValidationExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newSUnitExecutionPolicy

	^self sunitExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newSUnitTooExecutionPolicy

	^self sunitTooExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newSUnitVMBenchmarkExecutionPolicy

	^self sunitVMBenchmarkExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newSUnitVMExecutionPolicy

	^self sunitVMExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'private - execution policy' stamp: 'SqR 3/8/2015 18:10'!
newSUnitVMValidationExecutionPolicy

	^self sunitVMValidationExecutionPolicyClass new! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useAssessmentsBenchmarkExecutionPolicy

	self executionPolicy: self newAssessmentsBenchmarkExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useAssessmentsExecutionPolicy

	self executionPolicy: self newAssessmentsExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useAssessmentsValidationExecutionPolicy

	self executionPolicy: self newAssessmentsValidationExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useSUnitExecutionPolicy

	self executionPolicy: self newSUnitExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useSUnitTooExecutionPolicy

	self executionPolicy: self newSUnitTooExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useSUnitVMBenchmarkExecutionPolicy

	self executionPolicy: self newSUnitVMBenchmarkExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useSUnitVMExecutionPolicy

	self executionPolicy: self newSUnitVMExecutionPolicy! !

!ProtectedMessageSend methodsFor: 'execution policy' stamp: 'SqR 3/8/2015 18:10'!
useSUnitVMValidationExecutionPolicy

	self executionPolicy: self newSUnitVMValidationExecutionPolicy! !

!ProtectedMessageSend class methodsFor: 'instance creation' stamp: 'SqR 9/7/2015 17:30'!
new
	"Become independent of whether super new sends initialize"

	^self basicNew initialize! !

!ProtectedMessageSend class methodsFor: 'instance creation'!
receiver: aReceiver

	^self new
		receiver: aReceiver;
		yourself! !

!ProtectedMessageSend class methodsFor: 'instance creation'!
receiver: aReceiver selector: aSelector

	^(self receiver: aReceiver)
		selector: aSelector;
		yourself! !

!ProtectedMessageSend class methodsFor: 'instance creation'!
receiver: aReceiver selector: aSelector arguments: anArray

	^(self receiver: aReceiver selector: aSelector)
		arguments: anArray;
		yourself! !

!AttemptToStartPrerequisite methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	^self executionPolicy performAttemptToStartPrerequisite: self! !

!AttemptToStartPrerequisite methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
handle: anException inTheContextOf: anEvaluationContext

	anException
		occurredWhileEvaluatingPrerequisiteAction: self
		inTheContextOf: anEvaluationContext! !

!AttemptToStartPrerequisite methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newError

	^self prerequisiteStartErrorClass for: self! !

!AttemptToStartPrerequisite methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStartErrorClass

	^PrerequisiteStartFailure! !

!AttemptToStartPrerequisite methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
prerequisiteStopErrorClass

	^PrerequisiteStopFailure! !

!AttemptToStartPrerequisite class methodsFor: 'instance creation'!
prerequisite: anObject

	^self receiver: anObject! !

!AttemptToStopPrerequisite methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	^self executionPolicy performAttemptToStopPrerequisite: self! !

!AttemptToStopPrerequisite methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newError

	^self prerequisiteStopErrorClass for: self! !

!Check methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
aboutToEvaluateCheckInTheContextOf: anEvaluationContext

	self executionPolicy
		aboutToEvaluateCheck: self
		inTheContextOf: anEvaluationContext! !

!Check methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
doneEvaluatingCheckInTheContextOf: anEvaluationContext

	self executionPolicy
		doneEvaluatingCheck: self
		inTheContextOf: anEvaluationContext! !

!Check methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
fastPerformInTheContextOf: anEvaluationContext

	^self executionPolicy
		fastPerformCheck: self
		inTheContextOf: anEvaluationContext! !

!Check methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
performInTheContextOf: anEvaluationContext

	self executionPolicy
		performCheck: self
		inTheContextOf: anEvaluationContext! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
actualReceiver

	^actualReceiver! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
actualReceiver: anObject

	actualReceiver := anObject! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checkSelector

	^self selector! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checklist

	^self receiver! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
checklist: anObject

	self receiver: anObject! !

!Check methodsFor: 'private - accessing' stamp: 'SqR 3/8/2015 18:10'!
prerequisites

	^self executionPolicy prerequisitesFor: self! !

!Check methodsFor: 'private - converting' stamp: 'SqR 3/8/2015 18:10'!
asCheckOfClass: aCheckClass

	| answer |
	answer := aCheckClass
		checklist: self checklist
		checkSelector: self checkSelector.
	answer executionPolicy: self executionPolicy.
	^answer! !

!Check methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asDebugCheck

	^self asCheckOfClass: DebugCheck! !

!Check methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asRunToFailureCheck

	^self asCheckOfClass: RunToFailureCheck! !

!Check methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asSingleThreadRunToFailureCheck

	^self asCheckOfClass: SingleThreadRunToFailureCheck! !

!Check methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	self aboutToEvaluateCheckInTheContextOf: anEvaluationContext.
	[self performInTheContextOf: anEvaluationContext]
		ensure: [self doneEvaluatingCheckInTheContextOf: anEvaluationContext]! !

!Check methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicFastPassEvaluateInTheContextOf: anEvaluationContext

	self aboutToEvaluateCheckInTheContextOf: anEvaluationContext.
	^[
		| prettyPrintForPass |
		prettyPrintForPass := self fastPerformInTheContextOf: anEvaluationContext.
		prettyPrintForPass notNil ifTrue:
			[
				| newPass |
				newPass := self newFastCheckPassWith: prettyPrintForPass.
				anEvaluationContext addResult: newPass
			]
	]
		ensure: [self doneEvaluatingCheckInTheContextOf: anEvaluationContext]! !

!Check methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
ensurePrerequisitesInTheContextOf: anEvaluationContext

	^anEvaluationContext ensurePrerequisitesFor: self! !

!Check methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassEvaluateInTheContextOf: anEvaluationContext 

	^self executionPolicy fastPassEvaluate: self inTheContextOf: anEvaluationContext! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkExpectationFailedClass

	^CheckBenchmarkExpectationFailure! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkExpectationMetClass

	^CheckBenchmarkExpectationPass! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkMeasurementResultClass

	^CheckBenchmarkResult! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkBenchmarkPassClass

	^CheckBenchmarkPass! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkErrorClass

	^CheckError! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkFailureClass

	^CheckFailure! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkNoticeClass

	^CheckNotice! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkPassClass

	^CheckPass! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkSkipClass

	^CheckSkip! !

!Check methodsFor: 'private - result classes' stamp: 'SqR 3/8/2015 18:10'!
checkValidationFailureClass

	^CheckValidationFailure! !

!Check methodsFor: 'accessing' stamp: 'SqR 3/8/2015 18:10'!
checkExecutionCount

	^1! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckBenchmarkExpectationFailed

	^self checkBenchmarkExpectationFailedClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckBenchmarkExpectationMet

	^self checkBenchmarkExpectationMetClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckBenchmarkPass

	^self checkBenchmarkPassClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckBenchmarkResult

	^self checkBenchmarkMeasurementResultClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckError

	^self checkErrorClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckFailure

	^self checkFailureClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckNotice

	^self checkNoticeClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckPass

	^self checkPassClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckSkip

	^self checkSkipClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newCheckValidationFailure

	^self checkValidationFailureClass for: self! !

!Check methodsFor: 'private - results' stamp: 'SqR 3/8/2015 18:10'!
newFastCheckPassWith: aPrettyPrintString

	^self newCheckPass
		prettyPrint: aPrettyPrintString;
		yourself! !

!Check methodsFor: 'printing' stamp: 'SqR 3/8/2015 18:10'!
receiverPrintString

	^self receiver name! !

!Check class methodsFor: 'instance creation'!
checklist: aChecklist checkSelector: aSelector

	^self
		receiver: aChecklist
		selector: aSelector! !

!AbstractDebugCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicDebugInTheContextOf: anEvaluationContext

	self aboutToEvaluateCheckInTheContextOf: anEvaluationContext.
	[self debugPerformInTheContextOf: anEvaluationContext]
		ensure: [self doneEvaluatingCheckInTheContextOf: anEvaluationContext]! !

!AbstractDebugCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicRunToFailureInTheContextOf: anEvaluationContext

	self aboutToEvaluateCheckInTheContextOf: anEvaluationContext.
	[self runToFailurePerformInTheContextOf: anEvaluationContext]
		ensure: [self doneEvaluatingCheckInTheContextOf: anEvaluationContext]! !

!AbstractDebugCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
fastPassEvaluateInTheContextOf: anEvaluationContext
	"For the sake of simplicity, fast pass evaluation is not supported for debugging"

	^self evaluateInTheContextOf: anEvaluationContext! !

!AbstractDebugCheck methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
debugPerformInTheContextOf: anEvaluationContext

	self executionPolicy
		debugPerformCheck: self
		inTheContextOf: anEvaluationContext! !

!AbstractDebugCheck methodsFor: 'private - execution' stamp: 'SqR 3/8/2015 18:10'!
runToFailurePerformInTheContextOf: anEvaluationContext

	self executionPolicy
		runToFailurePerformCheck: self
		inTheContextOf: anEvaluationContext! !

!DebugCheck methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asDebugCheck

	^self! !

!DebugCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	^self basicDebugInTheContextOf: anEvaluationContext! !

!RunToFailureCheck methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asRunToFailureCheck

	^self! !

!RunToFailureCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
basicEvaluateInTheContextOf: anEvaluationContext

	^self basicRunToFailureInTheContextOf: anEvaluationContext! !

!RunToFailureCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
handle: anException inTheContextOf: anEvaluationContext

	anException
		occurredWhileRunning: self
		toFailureInTheContextOf: anEvaluationContext! !

!SingleThreadRunToFailureCheck methodsFor: 'converting' stamp: 'SqR 3/8/2015 18:10'!
asSingleThreadRunToFailureCheck

	^self! !

!SingleThreadRunToFailureCheck methodsFor: 'private - evaluation' stamp: 'SqR 3/8/2015 18:10'!
handle: anException inTheContextOf: anEvaluationContext

	anException
		occurredWhileRunning: self
		toFailureInTheContextOf: anEvaluationContext! !

!UndefinedObject methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 21:19'!
validationAspectPrettyPrint

	^'<no aspect>'! !

!Behavior methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 21:18'!
classForBrowsing

	^self! !

!String methodsFor: '*Assessments' stamp: 'sqr 1/16/2016 22:27'!
assessmentsPrettyPrint

	| input output useCaps next addSpace |
	addSpace := false.
	useCaps := true.
	input := self readStream.
	output := WriteStream on: self species new.
	[input atEnd] whileFalse:
		[
			next := input next.
			addSpace := next isUppercase.
			useCaps
				ifTrue: [next := next asUppercase.  useCaps := false]
				ifFalse: [next := next asLowercase].
			(addSpace and: [output position > 0]) ifTrue: [output nextPut: Character space].
			output nextPut: next
		].
	^output contents! !

!String methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 21:18'!
validationAspectPrettyPrint

	^self assessmentsPrettyPrint! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:54'!
handlerActionForEvaluation

	self pass! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:54'!
handlerActionForPrerequisiteEvaluation

	self pass! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:54'!
handlerActionForRunningToFailure

	self pass! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:55'!
messageTextForHandling

	| answer |
	answer := self messageText.
	(answer isNil or: [answer isEmpty]) ifTrue: [^self class name].
	^answer! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:55'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	self handlerActionForEvaluation! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 3/24/2015 19:03'!
occurredWhileEvaluatingPrerequisiteAction: aPrerequisiteAction
inTheContextOf: anEvaluationContext

	self preventsCheckSuccess ifTrue:
		[
			| error |
			error := aPrerequisiteAction newError.
			error prettyPrint: self messageTextForHandling.
			anEvaluationContext addResult: error
		].
	self handlerActionForPrerequisiteEvaluation! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 3/24/2015 19:03'!
occurredWhileRunning: aCheck
toFailureInTheContextOf: anEvaluationContext

	self preventsCheckSuccess ifTrue: [self handlerActionForRunningToFailure].
	self
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 3/24/2015 19:07'!
preventsCheckSuccess

	^false! !

!Exception methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:55'!
resignalAsAssessmentMalfunctionNotification

	| malfunction |
	malfunction := AssessmentMalfunctionNotification new.
	malfunction messageText: self messageTextForHandling.
	self resignalAs: malfunction! !

!Error methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:57'!
handlerActionForEvaluation

	self return! !

!Error methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:57'!
handlerActionForPrerequisiteEvaluation

	self return: false! !

!Error methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:56'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext

	| error |
	error := aCheck newCheckError.
	error prettyPrint: self messageTextForHandling.
	anEvaluationContext addResult: error.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!Error methodsFor: '*Assessments' stamp: 'SqR 3/24/2015 19:01'!
preventsCheckSuccess

	^true! !

!Notification methodsFor: '*Assessments' stamp: 'SqR 2/21/2015 20:55'!
handlerActionForPrerequisiteEvaluation

	self handlerActionForEvaluation! !

!UnhandledError methodsFor: '*Assessments' stamp: 'sqr 1/16/2016 22:30'!
handlerActionForEvaluation
	"In some dialects, UnhandledError is not a subclass of Error"

	self return! !

!UnhandledError methodsFor: '*Assessments' stamp: 'sqr 1/16/2016 22:30'!
handlerActionForPrerequisiteEvaluation
	"In some dialects, UnhandledError is not a subclass of Error"

	self return: false! !

!UnhandledError methodsFor: '*Assessments' stamp: 'sqr 1/16/2016 22:30'!
occurredWhileEvaluating: aCheck
inTheContextOf: anEvaluationContext
	"In some dialects, UnhandledError is not a subclass of Error"

	| error |
	error := aCheck newCheckError.
	error prettyPrint: self messageTextForHandling.
	anEvaluationContext addResult: error.
	super
		occurredWhileEvaluating: aCheck
		inTheContextOf: anEvaluationContext! !

!UnhandledError methodsFor: '*Assessments' stamp: 'sqr 1/16/2016 22:30'!
preventsCheckSuccess
	"In some dialects, UnhandledError is not a subclass of Error"

	^true! !
