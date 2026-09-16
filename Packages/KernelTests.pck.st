'From Cuis7.9 [latest update: #8189] on 16 September 2026 at 3:51:12 pm'!
'Description '!
!provides: 'KernelTests' 1 4!
SystemOrganization addCategory: #'KernelTests-Objects'!
SystemOrganization addCategory: #'KernelTests-Classes'!
SystemOrganization addCategory: #'KernelTests-Numbers'!
SystemOrganization addCategory: #'KernelTests-Text'!
SystemOrganization addCategory: #'KernelTests-Chronology'!
SystemOrganization addCategory: #'KernelTests-Methods'!
SystemOrganization addCategory: #'KernelTests-Exceptions'!
SystemOrganization addCategory: #'KernelTests-Processes'!
SystemOrganization addCategory: #'KernelTests-Finalization'!
SystemOrganization addCategory: #'KernelTests-Fixed Bugs'!
SystemOrganization addCategory: #'KernelTests-Experiments'!


!classDefinition: #MyTestNotification category: #'KernelTests-Exceptions'!
Notification subclass: #MyTestNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'MyTestNotification class' category: #'KernelTests-Exceptions'!
MyTestNotification class
	instanceVariableNames: ''!

!classDefinition: #MyResumableTestError category: #'KernelTests-Exceptions'!
Error subclass: #MyResumableTestError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'MyResumableTestError class' category: #'KernelTests-Exceptions'!
MyResumableTestError class
	instanceVariableNames: ''!

!classDefinition: #MyTestError category: #'KernelTests-Exceptions'!
Error subclass: #MyTestError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'MyTestError class' category: #'KernelTests-Exceptions'!
MyTestError class
	instanceVariableNames: ''!

!classDefinition: #ExceptionTester category: #'KernelTests-Exceptions'!
Object subclass: #ExceptionTester
	instanceVariableNames: 'log suiteLog iterationsBeforeTimeout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'ExceptionTester class' category: #'KernelTests-Exceptions'!
ExceptionTester class
	instanceVariableNames: ''!

!classDefinition: #BecomeTestExperiment category: #'KernelTests-Experiments'!
Object subclass: #BecomeTestExperiment
	instanceVariableNames: 'a b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Experiments'!
!classDefinition: 'BecomeTestExperiment class' category: #'KernelTests-Experiments'!
BecomeTestExperiment class
	instanceVariableNames: ''!

!classDefinition: #FinalizationTestObject category: #'KernelTests-Finalization'!
Object subclass: #FinalizationTestObject
	instanceVariableNames: 'finalizeAction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Finalization'!
!classDefinition: 'FinalizationTestObject class' category: #'KernelTests-Finalization'!
FinalizationTestObject class
	instanceVariableNames: ''!

!classDefinition: #DateAndTimeTest category: #'KernelTests-Chronology'!
TestCase subclass: #DateAndTimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'DateAndTimeTest class' category: #'KernelTests-Chronology'!
DateAndTimeTest class
	instanceVariableNames: ''!

!classDefinition: #DateTest category: #'KernelTests-Chronology'!
TestCase subclass: #DateTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'DateTest class' category: #'KernelTests-Chronology'!
DateTest class
	instanceVariableNames: ''!

!classDefinition: #DurationTest category: #'KernelTests-Chronology'!
TestCase subclass: #DurationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'DurationTest class' category: #'KernelTests-Chronology'!
DurationTest class
	instanceVariableNames: ''!

!classDefinition: #MonthTest category: #'KernelTests-Chronology'!
TestCase subclass: #MonthTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'MonthTest class' category: #'KernelTests-Chronology'!
MonthTest class
	instanceVariableNames: ''!

!classDefinition: #TimeTest category: #'KernelTests-Chronology'!
TestCase subclass: #TimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'TimeTest class' category: #'KernelTests-Chronology'!
TimeTest class
	instanceVariableNames: ''!

!classDefinition: #TimespanTest category: #'KernelTests-Chronology'!
TestCase subclass: #TimespanTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'TimespanTest class' category: #'KernelTests-Chronology'!
TimespanTest class
	instanceVariableNames: ''!

!classDefinition: #WeekTest category: #'KernelTests-Chronology'!
TestCase subclass: #WeekTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'WeekTest class' category: #'KernelTests-Chronology'!
WeekTest class
	instanceVariableNames: ''!

!classDefinition: #YearTest category: #'KernelTests-Chronology'!
TestCase subclass: #YearTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Chronology'!
!classDefinition: 'YearTest class' category: #'KernelTests-Chronology'!
YearTest class
	instanceVariableNames: ''!

!classDefinition: #BehaviorTest category: #'KernelTests-Classes'!
TestCase subclass: #BehaviorTest
	instanceVariableNames: 'unreferenced referenced'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'BehaviorTest class' category: #'KernelTests-Classes'!
BehaviorTest class
	instanceVariableNames: ''!

!classDefinition: #BehaviorTestSubclass category: #'KernelTests-Classes'!
BehaviorTest subclass: #BehaviorTestSubclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'BehaviorTestSubclass class' category: #'KernelTests-Classes'!
BehaviorTestSubclass class
	instanceVariableNames: ''!

!classDefinition: #CategorizerTest category: #'KernelTests-Classes'!
TestCase subclass: #CategorizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'CategorizerTest class' category: #'KernelTests-Classes'!
CategorizerTest class
	instanceVariableNames: ''!

!classDefinition: #ClassOrganizerTest category: #'KernelTests-Classes'!
CategorizerTest subclass: #ClassOrganizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'ClassOrganizerTest class' category: #'KernelTests-Classes'!
ClassOrganizerTest class
	instanceVariableNames: ''!

!classDefinition: #ClassTest category: #'KernelTests-Classes'!
TestCase subclass: #ClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'ClassTest class' category: #'KernelTests-Classes'!
ClassTest class
	instanceVariableNames: ''!

!classDefinition: #MetaclassTest category: #'KernelTests-Classes'!
TestCase subclass: #MetaclassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Classes'!
!classDefinition: 'MetaclassTest class' category: #'KernelTests-Classes'!
MetaclassTest class
	instanceVariableNames: ''!

!classDefinition: #ExceptionHandlingConditionTest category: #'KernelTests-Exceptions'!
TestCase subclass: #ExceptionHandlingConditionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'ExceptionHandlingConditionTest class' category: #'KernelTests-Exceptions'!
ExceptionHandlingConditionTest class
	instanceVariableNames: ''!

!classDefinition: #ExceptionTests category: #'KernelTests-Exceptions'!
TestCase subclass: #ExceptionTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'ExceptionTests class' category: #'KernelTests-Exceptions'!
ExceptionTests class
	instanceVariableNames: ''!

!classDefinition: #ProgressInitiationExceptionTest category: #'KernelTests-Exceptions'!
TestCase subclass: #ProgressInitiationExceptionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Exceptions'!
!classDefinition: 'ProgressInitiationExceptionTest class' category: #'KernelTests-Exceptions'!
ProgressInitiationExceptionTest class
	instanceVariableNames: ''!

!classDefinition: #EphemeronIdentityDictionaryTest category: #'KernelTests-Finalization'!
TestCase subclass: #EphemeronIdentityDictionaryTest
	instanceVariableNames: 'keys dict'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Finalization'!
!classDefinition: 'EphemeronIdentityDictionaryTest class' category: #'KernelTests-Finalization'!
EphemeronIdentityDictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #FinalizationRegistryTest category: #'KernelTests-Finalization'!
TestCase subclass: #FinalizationRegistryTest
	instanceVariableNames: 'finalizationRegistry finalizerThatSignals finalizerFired finalizerThatRaisesError'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Finalization'!
!classDefinition: 'FinalizationRegistryTest class' category: #'KernelTests-Finalization'!
FinalizationRegistryTest class
	instanceVariableNames: ''!

!classDefinition: #FinalizationTest category: #'KernelTests-Finalization'!
TestCase subclass: #FinalizationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Finalization'!
!classDefinition: 'FinalizationTest class' category: #'KernelTests-Finalization'!
FinalizationTest class
	instanceVariableNames: ''!

!classDefinition: #TestValueWithinFix category: #'KernelTests-Fixed Bugs'!
TestCase subclass: #TestValueWithinFix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Fixed Bugs'!
!classDefinition: 'TestValueWithinFix class' category: #'KernelTests-Fixed Bugs'!
TestValueWithinFix class
	instanceVariableNames: ''!

!classDefinition: #CompiledMethodTest category: #'KernelTests-Methods'!
TestCase subclass: #CompiledMethodTest
	instanceVariableNames: 'iv1 iv2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Methods'!
!classDefinition: 'CompiledMethodTest class' category: #'KernelTests-Methods'!
CompiledMethodTest class
	instanceVariableNames: ''!

!classDefinition: #LiteralRefLocatorTest category: #'KernelTests-Methods'!
TestCase subclass: #LiteralRefLocatorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Methods'!
!classDefinition: 'LiteralRefLocatorTest class' category: #'KernelTests-Methods'!
LiteralRefLocatorTest class
	instanceVariableNames: ''!

!classDefinition: #MessageTest category: #'KernelTests-Methods'!
TestCase subclass: #MessageTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Methods'!
!classDefinition: 'MessageTest class' category: #'KernelTests-Methods'!
MessageTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractNumberTest category: #'KernelTests-Numbers'!
TestCase subclass: #AbstractNumberTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'AbstractNumberTest class' category: #'KernelTests-Numbers'!
AbstractNumberTest class
	instanceVariableNames: ''!

!classDefinition: #FloatTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #FloatTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'FloatTest class' category: #'KernelTests-Numbers'!
FloatTest class
	instanceVariableNames: ''!

!classDefinition: #FractionTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #FractionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'FractionTest class' category: #'KernelTests-Numbers'!
FractionTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerDigitLogicTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #IntegerDigitLogicTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'IntegerDigitLogicTest class' category: #'KernelTests-Numbers'!
IntegerDigitLogicTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #IntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'IntegerTest class' category: #'KernelTests-Numbers'!
IntegerTest class
	instanceVariableNames: ''!

!classDefinition: #LargeNegativeIntegerTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #LargeNegativeIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'LargeNegativeIntegerTest class' category: #'KernelTests-Numbers'!
LargeNegativeIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #LargePositiveIntegerTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #LargePositiveIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'LargePositiveIntegerTest class' category: #'KernelTests-Numbers'!
LargePositiveIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #NumberTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #NumberTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'NumberTest class' category: #'KernelTests-Numbers'!
NumberTest class
	instanceVariableNames: ''!

!classDefinition: #SmallIntegerTest category: #'KernelTests-Numbers'!
AbstractNumberTest subclass: #SmallIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'SmallIntegerTest class' category: #'KernelTests-Numbers'!
SmallIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #RandomTest category: #'KernelTests-Numbers'!
TestCase subclass: #RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'RandomTest class' category: #'KernelTests-Numbers'!
RandomTest class
	instanceVariableNames: ''!

!classDefinition: #LaggedFibonacciRandomTest category: #'KernelTests-Numbers'!
RandomTest subclass: #LaggedFibonacciRandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'LaggedFibonacciRandomTest class' category: #'KernelTests-Numbers'!
LaggedFibonacciRandomTest class
	instanceVariableNames: ''!

!classDefinition: #ParkMiller88RandomTest category: #'KernelTests-Numbers'!
RandomTest subclass: #ParkMiller88RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'ParkMiller88RandomTest class' category: #'KernelTests-Numbers'!
ParkMiller88RandomTest class
	instanceVariableNames: ''!

!classDefinition: #ParkMiller93RandomTest category: #'KernelTests-Numbers'!
RandomTest subclass: #ParkMiller93RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Numbers'!
!classDefinition: 'ParkMiller93RandomTest class' category: #'KernelTests-Numbers'!
ParkMiller93RandomTest class
	instanceVariableNames: ''!

!classDefinition: #BecomeTest category: #'KernelTests-Objects'!
TestCase subclass: #BecomeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'BecomeTest class' category: #'KernelTests-Objects'!
BecomeTest class
	instanceVariableNames: ''!

!classDefinition: #BooleanTest category: #'KernelTests-Objects'!
TestCase subclass: #BooleanTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'BooleanTest class' category: #'KernelTests-Objects'!
BooleanTest class
	instanceVariableNames: ''!

!classDefinition: #IfNotNilTests category: #'KernelTests-Objects'!
TestCase subclass: #IfNotNilTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'IfNotNilTests class' category: #'KernelTests-Objects'!
IfNotNilTests class
	instanceVariableNames: ''!

!classDefinition: #ObjectTest category: #'KernelTests-Objects'!
TestCase subclass: #ObjectTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'ObjectTest class' category: #'KernelTests-Objects'!
ObjectTest class
	instanceVariableNames: ''!

!classDefinition: #SpecialSelectorsTest category: #'KernelTests-Objects'!
TestCase subclass: #SpecialSelectorsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'SpecialSelectorsTest class' category: #'KernelTests-Objects'!
SpecialSelectorsTest class
	instanceVariableNames: ''!

!classDefinition: #WeakMessageSendTest category: #'KernelTests-Objects'!
TestCase subclass: #WeakMessageSendTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Objects'!
!classDefinition: 'WeakMessageSendTest class' category: #'KernelTests-Objects'!
WeakMessageSendTest class
	instanceVariableNames: ''!

!classDefinition: #DelayTest category: #'KernelTests-Processes'!
TestCase subclass: #DelayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'DelayTest class' category: #'KernelTests-Processes'!
DelayTest class
	instanceVariableNames: ''!

!classDefinition: #MonitorTest category: #'KernelTests-Processes'!
TestCase subclass: #MonitorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'MonitorTest class' category: #'KernelTests-Processes'!
MonitorTest class
	instanceVariableNames: ''!

!classDefinition: #ProcessTerminateBug category: #'KernelTests-Processes'!
TestCase subclass: #ProcessTerminateBug
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'ProcessTerminateBug class' category: #'KernelTests-Processes'!
ProcessTerminateBug class
	instanceVariableNames: ''!

!classDefinition: #ProcessTerminateUnwindNonLocalReturn category: #'KernelTests-Processes'!
TestCase subclass: #ProcessTerminateUnwindNonLocalReturn
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'ProcessTerminateUnwindNonLocalReturn class' category: #'KernelTests-Processes'!
ProcessTerminateUnwindNonLocalReturn class
	instanceVariableNames: ''!

!classDefinition: #ProcessTest category: #'KernelTests-Processes'!
TestCase subclass: #ProcessTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'ProcessTest class' category: #'KernelTests-Processes'!
ProcessTest class
	instanceVariableNames: ''!

!classDefinition: #ProcessorTest category: #'KernelTests-Processes'!
TestCase subclass: #ProcessorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'ProcessorTest class' category: #'KernelTests-Processes'!
ProcessorTest class
	instanceVariableNames: ''!

!classDefinition: #SemaphoreTest category: #'KernelTests-Processes'!
TestCase subclass: #SemaphoreTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Processes'!
!classDefinition: 'SemaphoreTest class' category: #'KernelTests-Processes'!
SemaphoreTest class
	instanceVariableNames: ''!

!classDefinition: #CharacterTest category: #'KernelTests-Text'!
TestCase subclass: #CharacterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'CharacterTest class' category: #'KernelTests-Text'!
CharacterTest class
	instanceVariableNames: ''!

!classDefinition: #StringTest category: #'KernelTests-Text'!
TestCase subclass: #StringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'StringTest class' category: #'KernelTests-Text'!
StringTest class
	instanceVariableNames: ''!

!classDefinition: #SymbolTest category: #'KernelTests-Text'!
TestCase subclass: #SymbolTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'SymbolTest class' category: #'KernelTests-Text'!
SymbolTest class
	instanceVariableNames: ''!

!classDefinition: #UnicodeNormalizationTest category: #'KernelTests-Text'!
TestCase subclass: #UnicodeNormalizationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'UnicodeNormalizationTest class' category: #'KernelTests-Text'!
UnicodeNormalizationTest class
	instanceVariableNames: ''!

!classDefinition: #UnicodeStringsTest category: #'KernelTests-Text'!
TestCase subclass: #UnicodeStringsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'UnicodeStringsTest class' category: #'KernelTests-Text'!
UnicodeStringsTest class
	instanceVariableNames: ''!

!classDefinition: #UnicodeSymbolsTest category: #'KernelTests-Text'!
TestCase subclass: #UnicodeSymbolsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'UnicodeSymbolsTest class' category: #'KernelTests-Text'!
UnicodeSymbolsTest class
	instanceVariableNames: ''!

!classDefinition: #UnicodeTest category: #'KernelTests-Text'!
TestCase subclass: #UnicodeTest
	instanceVariableNames: 'bytesOfExample1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Text'!
!classDefinition: 'UnicodeTest class' category: #'KernelTests-Text'!
UnicodeTest class
	instanceVariableNames: ''!


!BecomeTestExperiment commentStamp: '<historical>' prior: 0!
To be used by BecomeTest!

!EphemeronIdentityDictionaryTest commentStamp: '' prior: 0!
SUnit tests for weak identity key dictionaries!

!FloatTest commentStamp: 'fbs 3/8/2004 22:13' prior: 0!
I provide a test suite for Float values. Examine my tests to see how Floats should behave, and see how to use them.!

!SmallIntegerTest commentStamp: 'fbs 3/8/2004 22:13' prior: 0!
I provide a test suite for SmallInteger values. Examine my tests to see how SmallIntegers should behave, and see how to use them.!

!BecomeTest commentStamp: '<historical>' prior: 0!
Tests that common uses of #become: and related methods don't crash the system when some method could resume execution on a 'becomed' instance.!

!SpecialSelectorsTest commentStamp: '<historical>' prior: 0!
Tests for proper behavior of Special Selectors, for example #@!

!ProcessTest commentStamp: 'ul 8/16/2011 11:35' prior: 0!
I hold test cases for generic Process-related behaviour.!

!SemaphoreTest commentStamp: 'tlk 5/5/2006 13:32' prior: 0!
A SemaphoreTest is sunit test for simple and multiEx semaphores

Instance Variables none; does not have common test fixture accross all tests (because its testing differenct sorts of semaphores (could refactor into muliple testcases if there were more test conditions.
!

!MyResumableTestError methodsFor: 'exceptionDescription' stamp: 'tfei 13/Jun/1999, 4:46:00 am (UTC)'!
isResumable

	^true! !

!ExceptionTester methodsFor: 'accessing' stamp: 'dtl 2/Jun/2004, 1:53:00 am (UTC)'!
basicANSISignaledExceptionTestSelectors

	^#( simpleIsNestedTest simpleOuterTest doubleOuterTest doubleOuterPassTest doublePassOuterTest simplePassTest simpleResignalAsTest simpleResumeTest simpleRetryTest simpleRetryUsingTest simpleReturnTest)! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 21/Oct/2004, 9:54:00 pm (UTC)'!
basicTestSelectors
	^ #(#simpleEnsureTest #simpleEnsureTestWithNotification #simpleEnsureTestWithUparrow #simpleEnsureTestWithError #signalFromHandlerActionTest #resumableFallOffTheEndHandler #nonResumableFallOffTheEndHandler #doubleResumeTest #simpleTimeoutWithZeroDurationTest #simpleTimeoutTest simpleNoTimeoutTest)! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:14:00 pm (UTC)'!
doSomethingElseString

	^'Do something else.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:14:00 pm (UTC)'!
doSomethingExceptionalString

	^'Do something exceptional.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:13:00 pm (UTC)'!
doSomethingString

	^'Do something.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:15:00 pm (UTC)'!
doYetAnotherThingString

	^'Do yet another thing.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 21/Oct/2004, 9:15:00 pm (UTC)'!
iterationsBeforeTimeout

	^ iterationsBeforeTimeout! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 21/Oct/2004, 9:16:00 pm (UTC)'!
iterationsBeforeTimeout: anInteger

	iterationsBeforeTimeout := anInteger! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 7/Jun/1999, 7:03:00 pm (UTC)'!
log

	log == nil
		ifTrue: [log := OrderedCollection new].
	^log! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:30:00 pm (UTC)'!
suiteLog

	suiteLog == nil
		ifTrue: [suiteLog := OrderedCollection new].
	^suiteLog! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 8/Jun/1999, 1:15:00 pm (UTC)'!
testString

	^'This is only a test.'! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 8/Jun/1999, 1:17:00 pm (UTC)'!
clearLog

	log := nil! !

!ExceptionTester methodsFor: 'logging' stamp: 'jmv 13/Mar/2012, 4:33:00 pm (UTC)'!
contents

	^( self log
		inject: (WriteStream on: (String new: 80))
		into: [ :result :item |
			result 
				newLine; 
				nextPutAll: item;
				yourself] ) contents! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 7/Jun/1999, 7:03:00 pm (UTC)'!
log: aString

	self log add: aString! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 13/Jun/1999, 3:07:00 am (UTC)'!
logTest: aSelector

	self suiteLog add: aSelector! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 8/Jun/1999, 1:38:00 pm (UTC)'!
logTestResult: aString

	| index |
	index := self suiteLog size.
	self suiteLog 
		at: index
		put: ((self suiteLog at: index), ' ', aString)! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:13:00 pm (UTC)'!
doSomething

	self log: self doSomethingString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:14:00 pm (UTC)'!
doSomethingElse

	self log: self doSomethingElseString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:14:00 pm (UTC)'!
doSomethingExceptional

	self log: self doSomethingExceptionalString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:15:00 pm (UTC)'!
doYetAnotherThing

	self log: self doYetAnotherThingString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:16:00 pm (UTC)'!
methodWithError

	MyTestError signal: self testString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 8/Jun/1999, 1:16:00 pm (UTC)'!
methodWithNotification

	MyTestNotification signal: self testString! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 2/Jun/2004, 1:51:00 am (UTC)'!
doubleOuterPassTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer.
			self doSomethingElse]]
			on: MyTestNotification
			do: [:ex | ex pass.
				self doSomethingExceptional]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 2/Jun/2004, 1:49:00 am (UTC)'!
doubleOuterTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer.
			self doSomethingExceptional]]
			on: MyTestNotification
			do: [:ex | ex outer.
				self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 2/Jun/2004, 1:52:00 am (UTC)'!
doublePassOuterTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex pass.
			self doSomethingExceptional]]
			on: MyTestNotification
			do: [:ex | ex outer.
				self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 5:27:00 am (UTC)'!
simpleIsNestedTest
	"uses resignalAs:"

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			ex isNested "expecting to detect handler in #runTest:"
				ifTrue:
					[self doYetAnotherThing.
					ex resignalAs: MyTestNotification new]]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tpr 28/May/2004, 1:50:00 am (UTC)'!
simpleOuterTest
	"uses #resume"

	[[self doSomething.
	MyTestNotification signal.
	"self doSomethingElse"
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer. self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 4:37:00 am (UTC)'!
simplePassTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex pass "expecting handler in #runTest:"]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 6:12:00 am (UTC)'!
simpleResignalAsTest
	"ExceptionTester new simpleResignalAsTest"

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do:
			[:ex | ex resignalAs: MyTestError new]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'nice 28/Dec/2009, 1:06:00 am (UTC)'!
simpleResumeTest
	"see if we can resume twice"
	
	[ | it |
	self doSomething.
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	]
		on: MyResumableTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex resume: 3]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 5:02:00 am (UTC)'!
simpleRetryTest

	| theMeaningOfLife |
	theMeaningOfLife := nil.
	[self doSomething.
	theMeaningOfLife == nil
		ifTrue: [MyTestError signal]
		ifFalse: [self doSomethingElse]]
			on: MyTestError
			do:
				[:ex |
				theMeaningOfLife := 42.
				self doYetAnotherThing.
				ex retry]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 5:03:00 am (UTC)'!
simpleRetryUsingTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex | ex retryUsing: [self doYetAnotherThing]]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 13/Jun/1999, 4:59:00 am (UTC)'!
simpleReturnTest

	| it |
	it :=
		[self doSomething.
		MyTestError signal.
		self doSomethingElse]
			on: MyTestError
			do: [:ex | ex return: 3].
	it = 3 ifTrue: [self doYetAnotherThing]! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 2/Jun/2004, 1:56:00 am (UTC)'!
doubleOuterPassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 2/Jun/2004, 1:56:00 am (UTC)'!
doublePassOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:09:00 am (UTC)'!
simpleIsNestedTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:10:00 am (UTC)'!
simpleOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:10:00 am (UTC)'!
simplePassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:11:00 am (UTC)'!
simpleResignalAsTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'RAA 8/Dec/2000, 4:59:00 pm (UTC)'!
simpleResumeTestResults

	"see if we can resume twice"

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:23:00 am (UTC)'!
simpleRetryTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingString;
			add: self doSomethingElseString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 5:23:00 am (UTC)'!
simpleRetryUsingTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 13/Jun/1999, 6:22:00 am (UTC)'!
simpleReturnTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 14/Nov/1999, 9:26:00 pm (UTC)'!
doubleResumeTest

       [self doSomething.
       MyResumableTestError signal.
       self doSomethingElse.
       MyResumableTestError signal.
       self doYetAnotherThing]
               on: MyResumableTestError
               do: [:ex | ex resume].! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 7/Jun/1999, 5:43:00 pm (UTC)'!
nonResumableFallOffTheEndHandler
	
	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 9/Jun/1999, 8:07:00 pm (UTC)'!
resumableFallOffTheEndHandler

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 19/Aug/1999, 5:39:00 am (UTC)'!
signalFromHandlerActionTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[self doYetAnotherThing.
			MyTestError signal]! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 8/Jun/1999, 1:44:00 pm (UTC)'!
simpleEnsureTest

	[self doSomething.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 8/Jun/1999, 4:50:00 pm (UTC)'!
simpleEnsureTestWithError

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 8/Jun/1999, 2:15:00 pm (UTC)'!
simpleEnsureTestWithNotification

	[self doSomething.
	self methodWithNotification.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 9/Jun/1999, 8:04:00 pm (UTC)'!
simpleEnsureTestWithUparrow

	[self doSomething.
	true ifTrue: [^nil].
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'jmv 8/May/2012, 1:38:00 am (UTC)'!
simpleNoTimeoutTest

	[ self doSomething ]
		valueWithin: 1 days onTimeout:
			[ self doSomethingElse ]! !

!ExceptionTester methodsFor: 'tests' stamp: 'brp 22/Oct/2004, 4:00:00 pm (UTC)'!
simpleTimeoutTest

	| n |
	[1 to: 1000000 do: [ :i | n := i. self doSomething ] ]
		valueWithin: 50 milliSeconds onTimeout:
			[ self iterationsBeforeTimeout: n.
			self doSomethingElse ]! !

!ExceptionTester methodsFor: 'tests' stamp: 'brp 22/Oct/2004, 4:00:00 pm (UTC)'!
simpleTimeoutWithZeroDurationTest

	[ self doSomething ]
		valueWithin: 0 seconds onTimeout:
			[ self doSomethingElse ].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 7/Jun/1999, 6:28:00 pm (UTC)'!
warningTest

	self log: 'About to signal warning.'.
	Warning signal: 'Ouch'.
	self log: 'Warning signal handled and resumed.'! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 14/Nov/1999, 9:29:00 pm (UTC)'!
doubleResumeTestResults

       ^OrderedCollection new
               add: self doSomethingString;
               add: self doSomethingElseString;
               add: self doYetAnotherThingString;
               yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/Jun/1999, 1:21:00 pm (UTC)'!
nonResumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 19/Aug/1999, 6:39:00 am (UTC)'!
resumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 19/Aug/1999, 5:51:00 am (UTC)'!
signalFromHandlerActionTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/Jun/1999, 1:47:00 pm (UTC)'!
simpleEnsureTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 9/Jun/1999, 9:44:00 pm (UTC)'!
simpleEnsureTestWithErrorResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/Jun/1999, 2:13:00 pm (UTC)'!
simpleEnsureTestWithNotificationResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/Jun/1999, 10:55:00 pm (UTC)'!
simpleEnsureTestWithUparrowResults

	^OrderedCollection new
		add: self doSomethingString;
"		add: self doSomethingElseString;"
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'brp 21/Oct/2004, 8:54:00 pm (UTC)'!
simpleNoTimeoutTestResults

	^OrderedCollection new
		add: self doSomethingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'jmv 6/Feb/2024, 2:18:21 pm (UTC)'!
simpleTimeoutTestResults

	| things |
	things := OrderedCollection new.

	self iterationsBeforeTimeout timesRepeat: [ things add: self  doSomethingString ].
	things add: self doSomethingElseString.

	^ things! !

!ExceptionTester methodsFor: 'results' stamp: 'brp 21/Oct/2004, 8:52:00 pm (UTC)'!
simpleTimeoutWithZeroDurationTestResults

	^OrderedCollection new
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 13/Jun/1999, 5:25:00 am (UTC)'!
runAllTests
	"ExceptionTester new runAllTests"

	self
		runBasicTests;
		runBasicANSISignaledExceptionTests! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 13/Jun/1999, 3:54:00 am (UTC)'!
runBasicANSISignaledExceptionTests

	self basicANSISignaledExceptionTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector]! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 9/Jun/1999, 8:06:00 pm (UTC)'!
runBasicTests

	self basicTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector]! !

!ExceptionTester methodsFor: 'testing' stamp: 'brp 21/Oct/2004, 9:40:00 pm (UTC)'!
runTest: aSelector

	| actualResult expectedResult |
	[ self 
		logTest: aSelector;
		clearLog;
		perform: aSelector ]
			on: MyTestError do: 
				[ :ex | self log: 'Unhandled Exception'.
					ex return: nil ].

	actualResult	:= self log.
	expectedResult := self perform: (aSelector, #Results) asSymbol.

	actualResult = expectedResult
		ifTrue: [self logTestResult: 'succeeded']
		ifFalse: [self logTestResult: 'failed' ].
! !

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 11/Jan/2019, 4:21:36 pm (UTC)'!
initialize
	a _ 1.
	b _ 2.! !

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 29/Jan/2019, 2:59:12 pm (UTC)'!
messStuffUp
"In a Workspace

BecomeTestExperiment twoVars.
e _ BecomeTestExperiment new.
e messStuffUp.

e _ nil. Smalltalk garbageCollect.
BecomeTestExperiment allInstances

Processor invalidMethodInStackDueToClass: BecomeTestExperiment
"

	BecomeTestExperiment threeVars. "New ivar c is added before a and be"
	^a + b! !

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 29/Jan/2019, 2:59:16 pm (UTC)'!
messStuffUpWBecome
"In a Workspace

BecomeTestExperiment twoVars.
e _ BecomeTestExperiment new.
e messStuffUpWBecome.

e _ nil. Smalltalk garbageCollect.
BecomeTestExperiment allInstances

Processor invalidMethodInStackDueToClass: BecomeTestExperiment
"

	self becomeForward: {22}.
	^a + b! !

!BecomeTestExperiment class methodsFor: 'as yet unclassified' stamp: 'jmv 26/Aug/2026, 12:52:15 am (UTC)'!
threeVars

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'c a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'KernelTests-Experiments'! !

!BecomeTestExperiment class methodsFor: 'as yet unclassified' stamp: 'jmv 26/Aug/2026, 12:52:21 am (UTC)'!
twoVars

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'KernelTests-Experiments'! !

!FinalizationTestObject methodsFor: 'accessing' stamp: 'jmv 17/Dec/2024, 1:20:13 pm (UTC)'!
finalizeAction: aBlock
	finalizeAction := aBlock! !

!FinalizationTestObject methodsFor: 'finalization' stamp: 'jmv 17/Dec/2024, 1:20:29 pm (UTC)'!
finalize
	finalizeAction value! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 5/Jun/2014, 6:29:00 pm (UTC)'!
testCreation
	"
	self new testCreation
	"
	| d |
	d _ (DateAndTime julianDayNumber: 2456814) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ (DateAndTime fromSeconds: 3579379200) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ DateAndTime fromSeconds: 3579431284.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	self assert: d hour = 14.
	self assert: d minute = 28.
	self assert: d second = 4.
	self assert: d nanoSecond = 0! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 6:53:59 pm (UTC)'!
testDayOfWeek

	self assert: '13 May 2017 ' asDate dayOfWeek = 6.
	self assert: '13 May 2017 ' asDate dayOfWeekName = #Saturday! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 11:22:53 pm (UTC)'!
testInvalidOperations

	self should: [ DateAndTime tomorrow - Week current ] raise: Error.
	self should: [ DateAndTime tomorrow - Date today ] raise: Error! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jpb 3/Aug/2019, 3:33:37 am (UTC)'!
testIsDateAndTime
	self assert: (DateAndTime now is: #DateAndTime).! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 11:19:21 pm (UTC)'!
testLessDateAndTime

	self assert: DateAndTime today - DateAndTime yesterday = 1 days.
	self assert: DateAndTime today - DateAndTime tomorrow = -1 days.
	self assert: DateAndTime tomorrow - DateAndTime yesterday = 2 days.! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 5/Jun/2014, 6:22:00 pm (UTC)'!
testOperations
	"
	self new testOperations
	"
	| d dt |
	dt _ DateAndTime now.

	d _ 1 hours.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 minutes.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 seconds.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ -1 hours.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 minutes.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 seconds.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 27/Apr/2016, 7:04:00 pm (UTC)'!
testToByDo
	"
	DateAndTimeTest new testToByDo
	"
	| end last start step |
	start _ DateAndTime midnight.
	end _ DateAndTime midnight + 2 hours.
	step _ 15 minutes.
	start to: end by: step do: [ :time | last _ time ].
	self assert: last = end! !

!DateTest methodsFor: 'test creation' stamp: 'jmv 16/Sep/2026, 6:03:53 pm (UTC)'!
testCreation

	| date |
	date := '2014/6/30' asDate.
	self assert: date yearNumber = 2014.
	self assert: date monthIndex = 6.
	self assert: date dayOfMonth = 30.

	date := '70/12/30' asDate.
	self assert: date yearNumber = 70.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date := '1970/12/30' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date := '12/30/1970' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	self should: ['30/12/1970' asDate] raise: Error.

	date := '4/5/6' asDate.
	self assert: date yearNumber = 4.
	self assert: date monthIndex = 5.
	self assert: date dayOfMonth = 6.

	date := '4/5/2006' asDate.
	self assert: date yearNumber = 2006.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 5.

	date := '15 April 1982' asDate.
	self assert: date yearNumber = 1982.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 15.! !

!DateTest methodsFor: 'test creation' stamp: 'jmv 16/Sep/2026, 6:15:55 pm (UTC)'!
testCreationFirstCentury

	"Special format: third element is four digits."
	self assert: (Date fromString: '04-15-0082') yearNumber = 82.

	"Special format: third element is four digits."
	self assert: (Date fromString: '25/Nov/0097') yearNumber = 97.! !

!DateTest methodsFor: 'test creation' stamp: 'jmv 16/Sep/2026, 6:46:28 pm (UTC)'!
testCreationLater20thCentury

	"Special format: third element is two digits, but greater than 31. Second element can not be a month."
	self assert: (Date readFrom: '04-15-82' readStream allow90sStyleStamps: true) yearNumber = 1982.
	
	"Special format: third element is two digits, but greater than 31. Second element can is a month."
	self assert: (Date readFrom: '25/Nov/97' readStream allow90sStyleStamps: true) yearNumber = 1997.! !

!DateTest methodsFor: 'test creation' stamp: 'jmv 16/Sep/2026, 6:18:15 pm (UTC)'!
testCreationNegativeYears

	self assert: (Date fromString: '-123456 April 15') yearNumber = -123456.
	self assert: (Date fromString: '-12345 April 15') yearNumber = -12345.
	self assert: (Date fromString: '-1234 April 15') yearNumber = -1234.
	self assert: (Date fromString: '-123 April 15') yearNumber = -123.
	self assert: (Date fromString: '-12 April 15') yearNumber = -12.
	self assert: (Date fromString: '-1 April 15') yearNumber = -1.
! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 13/May/2017, 11:23:12 pm (UTC)'!
testInvalidOperations

	self should: [ Date today - DateAndTime tomorrow ] raise: Error.
	self should: [ Date today - Week current ] raise: Error.! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 13/May/2017, 11:16:42 pm (UTC)'!
testLessDate

	self assert: Date today - Date yesterday = 1 days.
	self assert: Date today - Date tomorrow = -1 days.
	self assert: Date tomorrow start - Date yesterday start = 2 days.! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 13/May/2017, 6:25:42 pm (UTC)'!
testLessDuration

	self assert: '13 May 2017' asDate - 3 days = '10 May 2017' asDate! !

!DateTest methodsFor: 'test printing' stamp: 'jmv 16/Sep/2026, 6:16:06 pm (UTC)'!
testPrintingFirstCentury

	"Special format: third element is four digits."
	self assert: ((Date fromString: '04-15-0082') printString endsWith: '0082').
	
	"Special format: third element is four digits."
	self assert: ((Date fromString: '25/Nov/0097') printString endsWith: '0097').! !

!DateTest methodsFor: 'test printing' stamp: 'jmv 16/Sep/2026, 6:45:59 pm (UTC)'!
testPrintingLater20thCentury

	"Special format: third element is two digits, but greater than 31. Second element can not be a month."
	self assert: ((Date readFrom: '04-15-82' readStream allow90sStyleStamps: true) printString endsWith: '1982').
	
	"Special format: third element is two digits, but greater than 31. Second element can is a month."
	self assert: ((Date readFrom: '25/Nov/97' readStream allow90sStyleStamps: true) printString endsWith: '1997').! !

!DateTest methodsFor: 'test printing' stamp: 'jmv 16/Sep/2026, 6:20:14 pm (UTC)'!
testPrintingNegativeYears

	self assert: ((Date fromString: '-123456 April 15') printString endsWith: '-123456').
	self assert: ((Date fromString: '-12345 April 15') printString endsWith: '-12345').
	self assert: ((Date fromString: '-1234 April 15') printString endsWith: '-1234').
	self assert: ((Date fromString: '-123 April 15') printString endsWith: '-123').
	self assert: ((Date fromString: '-12 April 15') printString endsWith: '-12').
	self assert: ((Date fromString: '-1 April 15') printString endsWith: '-1').
! !

!DateTest methodsFor: 'test printing' stamp: 'jmv 16/Sep/2026, 6:21:27 pm (UTC)'!
testPrintingWideCenturies

	self assert: ((Date fromString: '123456 April 15') printString endsWith: '123456').
	self assert: ((Date fromString: '12345 April 15') printString endsWith: '12345').
	self assert: ((Date fromString: '1234 April 15') printString endsWith: '1234').
	self assert: ((Date fromString: '123 April 15') printString endsWith: '0123').
	self assert: ((Date fromString: '12 April 15') printString endsWith: '0012').
	self assert: ((Date fromString: '1 April 15') printString endsWith: '0001').
! !

!DurationTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 6:56:00 pm (UTC)'!
testPrintString

	self assert: (Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' ! !

!MonthTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 6:33:22 pm (UTC)'!
testCreation

	self assert: 'July 1998' asMonth yearNumber = 1998.
	self assert: 'July 1998' asMonth monthIndex = 7.
	self assert: 'July 1998' asMonth monthName = #July.

	self assert: '1998/7' asMonth yearNumber = 1998.
	self assert: '1998/7' asMonth monthIndex = 7.
	self assert: '1998/7' asMonth monthName = #July.! !

!MonthTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 6:55:20 pm (UTC)'!
testPrintString

	self assert: 'July 1998' asMonth printString = 'July 1998'.
	self assert: '1998/7' asMonth printString = 'July 1998'.
! !

!TimeTest methodsFor: 'tests' stamp: 'jmv 5/Jun/2014, 6:32:00 pm (UTC)'!
testCreation
	"
	self new testCreation
	"
	| t |
	
	t _ Time seconds: 52262.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.

	t _ Time seconds: 52262 nanoSeconds: 414712000.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.
	self assert: t nanoSecond = 414712000! !

!TimeTest methodsFor: 'tests' stamp: 'jmv 5/Jun/2014, 6:34:00 pm (UTC)'!
testEqual
	"
	self new testEqual
	"
	| t1 t2 |
	
	t1 _ Time seconds: 52262.
	t2 _ Time seconds: 52262 nanoSeconds: 0.
	self assert: t1 = t2.
	t2 _ Time seconds: 52262 nanoSeconds: 1234.
	self deny: t1 = t2.
	t2 _ Time seconds: 52263 nanoSeconds: 0.
	self deny: t1 = t2.! !

!TimeTest methodsFor: 'tests' stamp: 'jmv 11/Nov/2014, 1:26:00 pm (UTC)'!
testTimePartsArePositive
	self assert: (Time afterMidnight: -10 minutes) minute > 0.
	self assert: (Time afterMidnight: -10 minutes) hour = 23.
	self assert: (Time afterMidnight: -10 minutes) minute = 50.! !

!TimeTest methodsFor: 'Tests' stamp: 'jmv 28/May/2014, 2:46:00 am (UTC)'!
testSqueakInquiries
	| timewords |

	timewords := #(0.5 30 62 130 4000 10000 60000 86401) 
		collect: [ :ss | Time humanWordsForSecondsAgo: ss ].
	self assert: 
		timewords = #('a second ago' '30 seconds ago' 'a minute ago' '2 minutes ago' 
			'an hour ago' '2 hours ago' '16 hours ago' 'yesterday').! !

!TimespanTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:42:40 am (UTC)'!
testIsTimespan
	self assert: (Timespan current is: #Timespan).! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 7:29:29 pm (UTC)'!
testCreation

	| week |
	week _ '2008-W52' asWeek.
	self assert: week yearNumber = 2008.
	self assert: week weekNumber = 52.
	self assert: week start = '2008-12-22' asDate start.

	self should: [ '2008-W53' asWeek ] raise: Error.
	self should: [ '2009-W54' asWeek ] raise: Error.

	week _ '2009-W01' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 1.
	self assert: week start = '2008-12-29' asDate start.

	week _ '2009-W02' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 2.
	self assert: week start = '2009-1-5' asDate start.

	week _ '2009-W52' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 52.
	self assert: week start = '2009-12-21' asDate start.

	week _ '2009-W53' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 53.
	self assert: week start = '2009-12-28' asDate start.

	week _ '2010-W01' asWeek.
	self assert: week yearNumber = 2010.
	self assert: week weekNumber = 1.
	self assert: week start = '2010-1-4' asDate start.

	week _ '2010-W02' asWeek.
	self assert: week yearNumber = 2010.
	self assert: week weekNumber = 2.
	self assert: week start = '2010-1-11' asDate start.! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 7:28:58 pm (UTC)'!
testIncluding

	| week |
	week _ Week including: '12 May 2017 ' asDate start.
	self assert: week = '2017-W19' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '12 May 2017 ' asDate.
	self assert: week = '2017-W19' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '2010/1/3' asDate start.
	self assert: week = '2009W53' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '2010/1/3' asDate.
	self assert: week = '2009W53' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	self should: [ Week including: (Year including: Date today) ] raise: Error! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 11:22:16 pm (UTC)'!
testInvalidOperations

	self should: [ Week current - Date tomorrow ] raise: Error.
	self should: [ Week current - DateAndTime tomorrow ] raise: Error.! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 6:55:27 pm (UTC)'!
testPrintString

	{
		'2008-W52'.
		'2009-W01'.
		'2009-W02'.
		'2009-W52'.
		'2009-W53'.
		'2010-W01'.
		'2010-W02'.
	} do: [ :string |
		self assert: string asWeek printString = string ]! !

!WeekTest methodsFor: 'test class methods' stamp: 'jmv 13/May/2017, 6:58:57 pm (UTC)'!
testWeekDayNames

	self assert: (Week indexOfDay: #Sunday) = 7.
	self assert: (Week nameOfDay: 7) = #Sunday! !

!YearTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 7:19:06 pm (UTC)'!
testCreation

	self assert: (Year yearNumber: 1998) yearNumber = 1998.
	self assert: '2008' asYear yearNumber = 2008.
	self assert: '2008' asYear start = '2008/01/01' asDate start! !

!YearTest methodsFor: 'tests' stamp: 'jmv 13/May/2017, 7:17:42 pm (UTC)'!
testIncluding

	self assert: (Year including: '12 May 2017 ' asDate start) yearNumber = 2017.
	self assert: (Year including: (Week including: '12 May 2017 ' asDate start)) yearNumber = 2017! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:56:10 pm (UTC)'!
testAllSuperclassesUpToFailsWhenWantsToStopOnInvalidSuperclass

	| invalidSuperclass |
	
	invalidSuperclass := Collection.
	self
		should: [ SmallInteger allSuperclassesUpTo: invalidSuperclass ]
		raise: Error
		withExceptionDo: [:anError | 
			self assert: (nil superclassNotValidErrorDescriptionFor: invalidSuperclass) equals: anError messageText ].! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:53:44 pm (UTC)'!
testAllSuperclassesUpToPutsFirstSuperclassesFirst

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Number.
	
	self assert: superclasses size equals: 2.
	self assert: superclasses first equals: Integer.
	self assert: superclasses second equals: Number.! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:56:01 pm (UTC)'!
testAllSuperclassesUpToStopsOnRightClass

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Integer.
	
	self assert: superclasses size equals: 1.
	self assert: (superclasses includes: Integer).! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:55:49 pm (UTC)'!
testHighestClassImplementingReturnsReceiverWhenNoOtherSuperclassImplementsSelector

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #addFirst: ifNone: [ self fail ].
	
	self assert: higestClass equals: OrderedCollection.! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:55:45 pm (UTC)'!
testHighestClassImplementingReturnsTheTopSuperclassImplementingSelector

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #select: ifNone: [ self fail ].
	
	self assert: higestClass equals: Collection.! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 3/Aug/2019, 1:29:55 pm (UTC)'!
testHighestClassImplementingValuesIfNoneBlockWhenSelectorIsNotImplemented

	OrderedCollection highestClassImplementing: #x ifNone: [ ^self  ].
	
	self fail! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'jmv 31/Jul/2023, 3:55:32 pm (UTC)'!
testWithAllSuperclassesUpToIncludesReceiverClass

	| superclasses |
	
	superclasses := SmallInteger withAllSuperclassesUpTo: Number.
	
	self assert: superclasses size equals: 3.
	self assert: superclasses first equals: SmallInteger.
	self assert: superclasses second equals: Integer.
	self assert: superclasses third equals: Number.! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 28/Jun/2020, 3:57:28 pm (UTC)'!
testAllAccessToInstVarReturnsAccessReceiversBranch

	| references |

	"Just in case the hierarchy changes... not much of a chance but... - Hernan"
	self deny: (IdentitySet includesBehavior: Dictionary).
	self deny: (Dictionary includesBehavior: IdentitySet).
	self assert: (IdentitySet includesBehavior: Set).
	self assert: (Dictionary includesBehavior: Set).
	self assert: (IdentityDictionary includesBehavior: Dictionary).

	references := IdentitySet allAccessesTo: 'array'.

	self assert: (references noneSatisfy: [ :aReference | aReference methodClass = IdentityDictionary ])! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 9/Apr/2020, 8:17:49 pm (UTC)'!
testAllAccessToIsEmptyForVariableNotDefinedInClass

	self assert: (Object allAccessesTo: 'iv1') isEmpty! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 28/Jun/2020, 3:53:39 pm (UTC)'!
testAllBroadAccessToReturnsAccessInAllHierarchy

	| references |

	"Just in case the hierarchy changes... not much of a chance but... - Hernan"
	self deny: (IdentitySet includesBehavior: Dictionary).
	self deny: (Dictionary includesBehavior: IdentitySet).
	self assert: (IdentitySet includesBehavior: Set).
	self assert: (Dictionary includesBehavior: Set).

	references := IdentitySet allBroadAccessesTo: 'array'.

	self assert: (references anySatisfy: [ :aReference | aReference methodClass = Dictionary ])! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'jmv 31/Jul/2023, 3:55:53 pm (UTC)'!
testAllUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables

	| unreferencedVariables |
	
	unreferencedVariables := BehaviorTestSubclass allUnreferencedInstanceVariables.
	
	self assert: unreferencedVariables size equals: 1.
	self assert: (unreferencedVariables includes: 'unreferenced').! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 2/Aug/2018, 8:54:45 pm (UTC)'!
testHasReferencesToInstanceVariableNamedLooksInClassOnly

	self deny: (self class hasReferencesToInstanceVariableNamed: 'referenced').
	self assert: (BehaviorTestSubclass hasReferencesToInstanceVariableNamed: 'referenced').
	! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 2/Aug/2018, 8:54:48 pm (UTC)'!
testIsInstanceVariableNamedReferencedInHierarchyLooksReferencesInHierarchy

	self assert: (self class isInstanceVariableNamedReferencedInHierarchy: 'referenced').
	self deny: (self class isInstanceVariableNamedReferencedInHierarchy: 'unreferenced').
	! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 28/Jun/2020, 3:48:19 pm (UTC)'!
testProtoObjectAllRegularInstVarNamesDoesNotFail

	self
		shouldnt: [ ProtoObject allRegularInstVarNames ]
		raise: Error! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'jmv 31/Jul/2023, 3:55:40 pm (UTC)'!
testUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables

	| unreferencedVariables |
	
	unreferencedVariables := self class unreferencedInstanceVariables.
	
	self assert: unreferencedVariables size equals: 1.
	self assert: (unreferencedVariables includes: 'unreferenced').! !

!BehaviorTestSubclass methodsFor: 'test data' stamp: 'HAW 2/Aug/2018, 8:54:29 pm (UTC)'!
methodReferencingReferencedVariable

	^referenced  ! !

!BehaviorTestSubclass class methodsFor: 'as yet unclassified' stamp: 'HAW 28/Feb/2019, 11:18:42 pm (UTC)'!
isAbstract

	^true! !

!CategorizerTest methodsFor: 'assertions' stamp: 'HAW 24/Sep/2020, 11:46:47 pm (UTC)'!
assertAddsWithoutBlanks: aCategoryWithBlanks

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer addCategory: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	
	
	! !

!CategorizerTest methodsFor: 'assertions' stamp: 'HAW 24/Sep/2020, 11:46:54 pm (UTC)'!
assertClassifiesWithoutBlanks: aCategoryWithBlanks

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer classify: 1 under: aCategoryWithBlanks suppressIfDefault: false.
	
	self assert: (categorizer categoryOfElement: 1) equals: aCategoryWithBlanks withBlanksTrimmed 
	! !

!CategorizerTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 3:57:12 pm (UTC)'!
assertRenamesToWithoutBlanks: aCategoryWithBlanks

	| categorizer categoryToRename |
	
	categorizer := self createCategorizer.
	
	categoryToRename := 'someCategory'.
	categorizer classify: 1 under: categoryToRename suppressIfDefault: false.
	categorizer renameCategory: categoryToRename to: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	self deny: (categorizer categories includes: categoryToRename).
	self assert: (categorizer categoryOfElement: 1) equals: aCategoryWithBlanks withBlanksTrimmed.! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:57:50 pm (UTC)'!
testClassifiesWithoutLeadingBlanks

	self assertClassifiesWithoutBlanks: self categoryWithLeadingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:57:33 pm (UTC)'!
testClassifiesWithoutTrailingBlanks

	self assertClassifiesWithoutBlanks: self categoryWithTrailingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:56:18 pm (UTC)'!
testRemovesLeadingBlanksWhenAddingCategory

	self assertAddsWithoutBlanks: self categoryWithLeadingBlanks.
	
	! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:55:58 pm (UTC)'!
testRemovesTrailingBlanksWhenAddingCategory

	self assertAddsWithoutBlanks: self categoryWithTrailingBlanks.
	
	! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:58:11 pm (UTC)'!
testRenamesWithoutLeadingBlanks

	self assertRenamesToWithoutBlanks: self categoryWithLeadingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2020, 11:58:01 pm (UTC)'!
testRenamesWithoutTrailingBlanks

	self assertRenamesToWithoutBlanks: self categoryWithTrailingBlanks ! !

!CategorizerTest methodsFor: 'support' stamp: 'HAW 24/Sep/2020, 8:41:35 pm (UTC)'!
categoryWithLeadingBlanks

	^ ' withLeadingBlanks'! !

!CategorizerTest methodsFor: 'support' stamp: 'HAW 24/Sep/2020, 8:41:23 pm (UTC)'!
categoryWithTrailingBlanks

	^ 'withTrailingBlanks '! !

!CategorizerTest methodsFor: 'support' stamp: 'jmv 28/Jan/2025, 5:59:53 pm (UTC)'!
createCategorizer

	^ Categorizer empty! !

!ClassOrganizerTest methodsFor: 'support' stamp: 'jmv 28/Jan/2025, 5:59:59 pm (UTC)'!
createCategorizer

	^ ClassOrganizer empty! !

!ClassTest methodsFor: 'definition tests' stamp: 'HAW 24/Oct/2019, 1:34:44 pm (UTC)'!
testDefinitionReplacingCategoryWithReplacesTheCategoryWithTheGivenOne

	| classDefinition newCategory newCategoryDefinition |
	
	newCategory := 'NewCategory'.
	newCategoryDefinition := 'category: ''', newCategory, ''''.
	classDefinition := self class definitionReplacingCategoryWith: newCategory.
	
	self assert: (classDefinition includesSubString: newCategoryDefinition).
	self deny: (classDefinition includesSubString: self class category)
		

! !

!ClassTest methodsFor: 'definition tests' stamp: 'HAW 24/Oct/2019, 1:42:54 pm (UTC)'!
testDefinitionReplacingCategoryWithWorksWhenCategoryIsNil

	| classDefinition newCategory newCategoryDefinition currentCategory |
	
	newCategory := 'NewCategory'.
	newCategoryDefinition := 'category: ''', newCategory, ''''.
	currentCategory := self class category.
	[SystemOrganization removeElement: self class name.
	classDefinition := self class definitionReplacingCategoryWith: newCategory.
	
	self assert: (classDefinition includesSubString: newCategoryDefinition).
	self deny: (classDefinition includesSubString: self class category asString) ] 
		ensure: [
			SystemOrganization classify: self class name under: currentCategory ].

		

! !

!MetaclassTest methodsFor: 'tests' stamp: 'HAW 17/Jul/2023, 4:32:33 pm (UTC)'!
testHasSubclassesReturnsThisClassHasSubclasses

	self assert: Object hasSubclasses equals: Object class hasSubclasses.
	self assert: self class hasSubclasses equals: self class class hasSubclasses ! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 29/Mar/2017, 5:50:30 pm (UTC)'!
testBlockClosuresCanBeUsedAsHandlingCondition

	self 
		shouldnt: [ [ Error signal ] on: [:anException | true ] do: [ :anError | ] ]
		raise: Error.
		
	! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 29/Mar/2017, 5:48:37 pm (UTC)'!
testFilterConditionWorksAsExcpetedWhenHandlingException

	self
		shouldnt: [ [ Error signal ] on: Error - ZeroDivide do: [ :anError |  ] ]
		raise: Error.
		
	self
		should: [ [ ZeroDivide signal ] on: Error - ZeroDivide do: [ :anError | self fail ]]
		raise: Error.

! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 29/Mar/2017, 5:49:45 pm (UTC)'!
testOrConditionWorksAsExcpetedWhenHandlingException

	self 
		shouldnt: [ [ Error signal ] on: Error, Notification do: [ :anError | ] ]
		raise: Error.
		
	self 
		shouldnt: [ [ Notification signal ] on: Error, Notification do: [ :anError |  ] ]
		raise: Notification 

	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:06 pm (UTC)'!
testCanCreateFilterConditionWithExceptionTypeAndExceptionType

	| condition |
	
	condition := Error - ZeroDivide .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:15 pm (UTC)'!
testCanCreateFilterConditionWithExceptionTypeAndFilterCondition

	| condition |
	
	condition := Error - (ZeroDivide - Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Halt new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:10 pm (UTC)'!
testCanCreateFilterConditionWithExceptionTypeAndOrCondition

	| condition |
	
	condition := Error - (ZeroDivide, Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self deny: (condition handles: Halt new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:35 pm (UTC)'!
testCanCreateFilterConditionWithFilterConditionAndExceptionType

	| condition |
	
	condition := (Error - Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:43 pm (UTC)'!
testCanCreateFilterConditionWithFilterConditionAndFilterCondition

	| condition |
	
	condition := (Error - Halt) - (ZeroDivide - Notification).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Notification new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:39 pm (UTC)'!
testCanCreateFilterConditionWithFilterConditionAndOrCondition

	| condition |
	
	condition := (Error - Halt) - (Notification, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: Notification new).
	self deny: (condition handles: ZeroDivide new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:20 pm (UTC)'!
testCanCreateFilterConditionWithOrConditionAndExceptionType

	| condition |
	
	condition := (Error, Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:29 pm (UTC)'!
testCanCreateFilterConditionWithOrConditionAndFilterCondition

	| condition |
	
	condition := (Error, Halt) - (ZeroDivide - ArithmeticError).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 29/Mar/2017, 7:24:25 pm (UTC)'!
testCanCreateFilterConditionWithOrConditionAndOrCondition

	| condition |
	
	condition := (Error, Halt) - (ArithmeticError, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 7:21:29 pm (UTC)'!
testCanCreateOrConditionWithExceptionTypeAndExceptionType

	| handlingCondition |
	
	handlingCondition := Error, Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 9:59:39 pm (UTC)'!
testCanCreateOrConditionWithExceptionTypeAndFilterCondition
	
	| handlingCondition |
	
	handlingCondition := Error, (ArithmeticError - ZeroDivide).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: ArithmeticError new).
	self deny: (handlingCondition handles: ZeroDivide new).
	
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 7:21:37 pm (UTC)'!
testCanCreateOrConditionWithExceptionTypeAndOrCondition

	| handlingCondition |
	
	handlingCondition := Error, (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 9:56:49 pm (UTC)'!
testCanCreateOrConditionWithFilterConditionAndExceptionType

	| condition |
	
	condition := (Error - ZeroDivide), ArithmeticError .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 9:56:57 pm (UTC)'!
testCanCreateOrConditionWithFilterConditionAndFilterCondition

	| condition |
	
	condition := (Error - ZeroDivide), (ArithmeticError - DomainError).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: DomainError new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 9:57:02 pm (UTC)'!
testCanCreateOrConditionWithFilterConditionAndOrCondition

	| condition |
	
	condition := (ArithmeticError - ZeroDivide), (Error, Halt).
	
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 7:22:01 pm (UTC)'!
testCanCreateOrConditionWithOrConditionAndExceptionType
	
	| handlingCondition |
	
	handlingCondition := (Error, Halt), Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 9:57:09 pm (UTC)'!
testCanCreateOrConditionWithOrConditionAndFilterCondition

	| exceptionSet |
	
	exceptionSet := (Error, Notification), (ArithmeticError - ZeroDivide).
	
	self assert: (exceptionSet handles: Error new).
	self assert: (exceptionSet handles: Notification new).
	self assert: (exceptionSet handles: ArithmeticError new).
	self deny: (exceptionSet handles: ZeroDivide new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 29/Mar/2017, 7:22:06 pm (UTC)'!
testCanCreateOrConditionWithOrConditionAndOrCondition

	| handlingCondition |
	
	handlingCondition := (Error, Halt), (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'environment preconditions' stamp: 'sqr 26/Jun/2019, 3:50:31 pm (UTC)'!
testExceptionsFollowTheExpectedHierarchy

	"This test exits because all the others use existing exceptions and assume a subclass relationship
	If the relationship is not the assumed one, false positives or negatives could happen"
	
	self assert: (Error inheritsFrom: Exception).
	self assert: (Notification inheritsFrom: Exception).
	self assert: (UnhandledError inheritsFrom: Exception).
	self assert: (Halt inheritsFrom: Exception).
	self assert: (ArithmeticError inheritsFrom: Error).
	self assert: (DomainError inheritsFrom: ArithmeticError).
	self assert: (ArithmeticMessageError inheritsFrom: Error).
	self assert: (NegativePowerError inheritsFrom: ArithmeticMessageError).
	self assert: (ZeroDivide inheritsFrom: ArithmeticMessageError)! !

!ExceptionTests methodsFor: 'private' stamp: 'md 26/Mar/2003, 3:40:00 am (UTC)'!
assertSuccess: anExceptionTester
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 2/Jun/2004, 1:54:00 am (UTC)'!
testDoubleOuterPass
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 2/Jun/2004, 1:54:00 am (UTC)'!
testDoublePassOuter
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:43:00 am (UTC)'!
testDoubleResume
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:44:00 am (UTC)'!
testNonResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 10/Oct/2015, 8:22:00 pm (UTC)'!
testResignalAs

	| answer |
	answer := [
		[3 zork]
			on: ZeroDivide
			do: [:ex | ex return: 5]
	] on: Error do: [:ex | ex resignalAs: ZeroDivide].
	self assert: answer == 5! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 15/Nov/2015, 3:42:00 pm (UTC)'!
testResignalAsUnwinds

	| unwound answer |
	unwound := false.
	answer := [
		[3 zork]
			on: ZeroDivide do: [:ex | self assert: unwound.  ex return: 5]
	] on: Error do: [:ex | [ex resignalAs: ZeroDivide] ifCurtailed: [unwound := true]].
	self assert: answer == 5! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:44:00 am (UTC)'!
testResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:44:00 am (UTC)'!
testSignalFromHandlerActionTest
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:48:00 am (UTC)'!
testSimpleEnsure
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:45:00 am (UTC)'!
testSimpleEnsureTestWithError
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:46:00 am (UTC)'!
testSimpleEnsureTestWithNotification
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:45:00 am (UTC)'!
testSimpleEnsureTestWithUparrow
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:46:00 am (UTC)'!
testSimpleIsNested
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:41:00 am (UTC)'!
testSimpleOuter
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:42:00 am (UTC)'!
testSimplePass
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:43:00 am (UTC)'!
testSimpleResignalAs
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:48:00 am (UTC)'!
testSimpleResume
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:48:00 am (UTC)'!
testSimpleRetry
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:47:00 am (UTC)'!
testSimpleRetryUsing
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 26/Mar/2003, 3:48:00 am (UTC)'!
testSimpleReturn
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest ) ! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'SqR 29/Aug/2014, 2:45:00 am (UTC)'!
testHandlerFromAction
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [
		[
			[self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
		] on: Error do: [ :ex | 3 / 0 ]
	] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer'! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 2/Jun/2004, 1:59:00 am (UTC)'!
testNonResumableOuter

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 2/Jun/2004, 2:00:00 am (UTC)'!
testNonResumablePass

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 15/Nov/2010, 3:39:00 pm (UTC)'!
testResumableOuter

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result = 5
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 15/Nov/2010, 3:39:00 pm (UTC)'!
testResumablePass

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
	self assert: result = 4
! !

!ExceptionTests methodsFor: 'testing' stamp: 'brp 21/Oct/2004, 8:42:00 pm (UTC)'!
testNoTimeout
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest ) ! !

!ExceptionTests methodsFor: 'testing' stamp: 'brp 21/Oct/2004, 8:41:00 pm (UTC)'!
testTimeoutWithZeroDuration
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest ) ! !

!ProgressInitiationExceptionTest methodsFor: 'testing' stamp: 'jmv 1/Mar/2025, 9:35:42 pm (UTC)'!
test01
	| countStorage |
	countStorage := {0}.
	self
		should: [ self sampleProgress: countStorage ]
		raise: ZeroDivide.
	self assert: countStorage first = 3.
	! !

!ProgressInitiationExceptionTest methodsFor: 'testing' stamp: 'jmv 1/Mar/2025, 9:36:37 pm (UTC)'!
test02
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[self sampleProgress: countStorage]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
		raise: Error.
	self assert: countStorage first = 4.
	! !

!ProgressInitiationExceptionTest methodsFor: 'testing' stamp: 'jmv 1/Mar/2025, 9:36:43 pm (UTC)'!
test03
	| countStorage |
	countStorage := {0}.
	self
		should: [
			[self sampleProgress: countStorage]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
		raise: Error.
	self assert: countStorage first = 3.
	! !

!ProgressInitiationExceptionTest methodsFor: 'testing' stamp: 'jmv 1/Mar/2025, 9:36:48 pm (UTC)'!
test04
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[
				[self sampleProgress: countStorage]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
		raise: Error.
	self assert: countStorage first = 4.
	! !

!ProgressInitiationExceptionTest methodsFor: 'testing' stamp: 'jmv 1/Mar/2025, 9:31:04 pm (UTC)'!
test05
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[
				[self sampleProgress: countStorage]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
		raise: Error.
	self assert: countStorage first = 4.
	! !

!ProgressInitiationExceptionTest methodsFor: 'aux' stamp: 'jmv 1/Mar/2025, 9:35:15 pm (UTC)'!
sampleProgress: countStorage
"
self new sampleProgress: {0}
"
	| d |
	d := Delay forMilliseconds: 10.
	^'Now here''s some Real Progress'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: 4
		during: [ :barBlock |
			1 to: 4 do: [ :x |
				countStorage at: 1 put: countStorage first + 1.
				barBlock value: x.
				d wait.
				x = 3 ifTrue: [1/0].	"just to make life interesting"
			].
			'done'
		].

! !

!EphemeronIdentityDictionaryTest methodsFor: 'running' stamp: 'JMM 6/Dec/2024, 2:38:47 pm (UTC)'!
setUp
	super setUp.
	keys := (1 to: 1000) collect:[:n| 'key', n asString].
	dict := EphemeronIdentityDictionary new.
	keys do:[:n| dict at: n put: n,n].
	super setUp! !

!EphemeronIdentityDictionaryTest methodsFor: 'running' stamp: 'JMM 3/Dec/2024, 8:07:10 pm (UTC)'!
triggerFinalization

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.! !

!EphemeronIdentityDictionaryTest methodsFor: 'coverage' stamp: '9/24/2024 24/Sep/2024, 10:45:00 am (UTC)'!
classToBeTested

	^ EphemeronIdentityDictionary! !

!EphemeronIdentityDictionaryTest methodsFor: 'tests' stamp: 'jmv 24/Dec/2024, 5:38:07 pm (UTC)'!
testFinalizeValuesWhenLastChainContinuesAtFront
	| objectWithHashModulo dictionary capacity a b c |
	objectWithHashModulo :=
		[ :requestedHash :modulo |
			| object allOfThem tries |
			tries := 0.
			allOfThem := OrderedCollection new.
			[
				tries := tries + 1.
				tries > 100 ifTrue: [ ^self ].
				object := Object new.
				allOfThem add: object.
				object hash \\ modulo = requestedHash ] whileFalse.
			allOfThem removeAll.
			object ].

	dictionary := self classToBeTested new.
	capacity := (dictionary instVarNamed: 'array') size.
	a := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: a put: 1.
	b := objectWithHashModulo value: capacity - 1 value: capacity.
	dictionary at: b put: 2.
	c := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: c put: 3.
	self assert: (dictionary instVarNamed: 'array') size equals: capacity.
	self assert: (dictionary array at: capacity - 1) key == a.
	self assert: (dictionary array at: capacity) key == b.
	self assert: (dictionary array at: 1) key == c.
	a := nil.
		self triggerFinalization.

	self assert: (dictionary includesKey: b).
	self assert: (dictionary includesKey: c).
	self assert: dictionary size equals: 2! !

!EphemeronIdentityDictionaryTest methodsFor: 'tests' stamp: 'JMM 3/Dec/2024, 7:09:56 pm (UTC)'!
testGrow
	keys := nil.
	dict at: self put: nil.
		self triggerFinalization.

	"Keys are gone but not yet finalized."
	dict grow! !

!EphemeronIdentityDictionaryTest methodsFor: 'tests' stamp: 'JMM 4/Dec/2024, 2:03:22 pm (UTC)'!
testNoNils
	| d |
	d := EphemeronIdentityDictionary new
	at: 'hello' copy put: 'world';
		yourself.
		self triggerFinalization.
	self deny: (d keys includes: nil)! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 28/Nov/2024, 3:20:54 pm (UTC)'!
testAddGC
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: (finalizationRegistry size = 1).
	self assert: (finalizationRegistry isEmpty not).
	x := nil.
	self triggerFinalization.
	self assert: (finalizationRegistry size = 0).
	self assert: (finalizationRegistry isEmpty).! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 29/Nov/2024, 12:28:48 pm (UTC)'!
testDo
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	finalizationRegistry keysDo: [:v | self assert: v = x]! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 6/Dec/2024, 2:39:00 pm (UTC)'!
testEKeyDictionary
	
	| d x |
	d := EphemeronIdentityDictionary new.
	x := Object new.
	d at: x put: 5.
	x := nil.
	self triggerFinalization.
	self assert: d isEmpty
! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'jmv 19/Dec/2024, 12:54:29 am (UTC)'!
testFinalizationWithError
	| reference |

	"This test can not run without the dev environment:
	It is expected to raise an UnhandledException, and the trick at the end to close
	the debugger can only be done if #isDevelopmentEnvironmentPresent"
	Smalltalk isDevelopmentEnvironmentPresent ifFalse:
		[ ^self ].

	finalizationRegistry
		add: Object new
		executor: finalizerThatRaisesError.
	finalizationRegistry
		add: (reference := Object new)
		executor: finalizerThatSignals.

	self triggerFinalization.
	self deny: finalizerFired.
	reference := nil.
	self triggerFinalization.
	self assert: finalizerFired.
	UISupervisor whenUIinSafeState:
		[DebuggerWindow closeAllDebuggersSuchThat:
			[:dbgWindow| '*Ephemeron Mourning*' match: dbgWindow label]]! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 29/Nov/2024, 12:34:57 pm (UTC)'!
testLegacy 
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	x := nil.
	finalizationRegistry finalizeValues! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 6/Dec/2024, 6:47:26 pm (UTC)'!
testRemove
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	finalizationRegistry removeKey: x ifAbsent: [self assert: false].
	finalizationRegistry removeKey: x ifAbsent: [^self assert: true].
	self assert: false.
! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 29/Nov/2024, 12:52:39 pm (UTC)'!
testRemoveAll
	| x y |

	
	x := Object new.
	finalizationRegistry add: x.
	y := Object new.
	finalizationRegistry add: y.
	self assert: finalizationRegistry size = 2.
	finalizationRegistry removeAll.
	self assert: finalizationRegistry size = 0.
! !

!FinalizationRegistryTest methodsFor: 'tests' stamp: 'JMM 29/Nov/2024, 12:49:29 pm (UTC)'!
testkey
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	self assert: (finalizationRegistry keys size = 1).
	finalizationRegistry keys do: [:k | self assert: k = x].
	! !

!FinalizationRegistryTest methodsFor: 'running' stamp: 'eem 19/Dec/2024, 12:44:35 am (UTC)'!
setUp

	super setUp.

	finalizationRegistry := FinalizationRegistry new.

	finalizerFired := false.
	finalizerThatSignals := ObjectFinalizer new receiver: [
		finalizerFired := true ] selector: #value.

	finalizerThatRaisesError := ObjectFinalizer new receiver: [ 
		Error signal ] selector: #value.! !

!FinalizationRegistryTest methodsFor: 'helpers' stamp: 'JMM 29/Nov/2024, 7:00:32 pm (UTC)'!
triggerFinalization

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.! !

!FinalizationTest methodsFor: 'tests' stamp: 'jmv 17/Dec/2024, 1:15:39 pm (UTC)'!
testFinalization
	| object finalizationWasRun finalizationAction |
	object := Object new.
	finalizationWasRun := false.
	finalizationAction := [ finalizationWasRun := true ].
	object toFinalizeSend: #value to: finalizationAction.
	self deny: finalizationWasRun.
	object := nil.
	self triggerFinalization.
	self assert: finalizationWasRun.! !

!FinalizationTest methodsFor: 'tests' stamp: 'jmv 17/Dec/2024, 1:22:59 pm (UTC)'!
testFinalization2
	| object finalizationWasRun finalizationAction |
	object := FinalizationTestObject new.
	finalizationWasRun := false.
	finalizationAction := [ finalizationWasRun := true ].
	object finalizeAction: finalizationAction.
	object finalizationRegistry add: object.
	self deny: finalizationWasRun.
	object := nil.
	self triggerFinalization.
	self assert: finalizationWasRun.! !

!FinalizationTest methodsFor: 'helpers' stamp: 'jmv 17/Dec/2024, 1:15:15 pm (UTC)'!
triggerFinalization

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'jmv 19/Jul/2025, 4:00:32 pm (UTC)'!
testValueWithinNonLocalReturnFixReal
	"self run: #testValueWithinNonLocalReturnFixReal"
	"The real test for the fix is just as obscure as the original problem"
	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time localMillisecondClock.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time localMillisecondClock - startTime.
		self deny: deltaTime < 90 description: 'deltaTime was: ', deltaTime printString.
	].
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 17/Aug/2007, 5:38:00 pm (UTC)'!
testValueWithinNonLocalReturnFixSimply
	"self run: #testValueWithinNonLocalReturnFixSimply"
	"The simple version to test the fix"
	self valueWithinNonLocalReturn.
	self shouldnt:[(Delay forMilliseconds: 50) wait] raise: TimedOut.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'KenD 25/Jun/2022, 1:29:49 am (UTC)'!
testValueWithinTimingBasic
	"Test timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'KenD 25/Jun/2022, 1:29:29 am (UTC)'!
testValueWithinTimingNestedInner
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'KenD 25/Jun/2022, 1:30:07 am (UTC)'!
testValueWithinTimingNestedOuter
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[
			3 timesRepeat: [
				[5 seconds asDelay wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.
	! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'KenD 25/Jun/2022, 1:30:22 am (UTC)'!
testValueWithinTimingRepeat
	"Test timing of valueWithin:onTimeout:"
	| time |
	[time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 400 milliSeconds.
	] forkAt: Processor userInterruptPriority.
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 17/Aug/2007, 5:37:00 pm (UTC)'!
valueWithinNonLocalReturn
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].
! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:03:52 am (UTC)'!
test01withPropertiesDoIfSelectorEvaluatesFirstBlockWhenMethodHasProperties

	| thisMethod propertyName methodProperties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.

	thisMethod 
		withPropertiesDo: [ :properties | methodProperties := properties ]
		ifSelector: [ self fail ].
	
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: (methodProperties at: propertyName)! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:04:31 am (UTC)'!
test02removePropertiesRestoresPenultimalLiteralToSelector

	| thisMethod propertyName |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:04:52 am (UTC)'!
test03removePropertiesKeepsSelectorIfMethodDoesNotHaveProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	"I have to remove it twice because when running with live typing methods always have method properties - Hernan"
	thisMethod removeProperties.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:05:22 am (UTC)'!
test04withPropertiesDoIfSelectorEvaluatesSecondBlockWhenMethodHasNoProperties

	| thisMethod methodSelector |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod 
		withPropertiesDo: [ :properties | self fail ]
		ifSelector: [ :selector | methodSelector := selector ].
		
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: methodSelector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:05:40 am (UTC)'!
test05hasLiteralSuchThatTravelsListeralsWhenMethodHasProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:05:54 am (UTC)'!
test06hasLiteralSuchThatTravelsListeralsWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:06:51 am (UTC)'!
test0701withPropertiesDoReturnsNilIfMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.

	self assert: (thisMethod withPropertiesDo: [ :properties | self fail ]) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:07:21 am (UTC)'!
test07withPropertiesDoEvaluatesBlockOnlyWhenMethodHasProperties

	| thisMethod propertyName methodProperties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.

	thisMethod withPropertiesDo: [ :properties | methodProperties := properties ].
	
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: (methodProperties at: propertyName)! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 7/Jul/2025, 9:22:31 pm (UTC)'!
test08hasLiteralReturnTrueWhenMethodHasPropertiesAndHasLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteral: #class).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 7/Jul/2025, 9:22:44 pm (UTC)'!
test09hasLiteralReturnFalseWhenMethodHasPropertiesAndHasNoLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self deny: (thisMethod hasLiteral: 'abc' asSymbol).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 7/Jul/2025, 9:22:51 pm (UTC)'!
test10hasLiteralReturnTrueWhenMethodHasNoPropertiesAndHasLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteral: #class).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 7/Jul/2025, 9:22:58 pm (UTC)'!
test11hasLiteralReturnFalseWhenMethodHasNoPropertiesAndHasNoLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self deny: (thisMethod hasLiteral: 'abc' asSymbol).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:08:51 am (UTC)'!
test12pragmaAtReturnsPragmaIfExist

	<test>
	
	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) key equals: #test! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:09:07 am (UTC)'!
test13pragmaAtReturnsNilIfPragmaDoesNotExist

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:09:26 am (UTC)'!
test14pragmasReturnsCollectionOfMethodPragmas

	<test>
	
	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas size equals: 1.
	self assert: pragmas first key equals: #test! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:09:49 am (UTC)'!
test15pragmasReturnsEmptyCollectionWhenMethodHasNoProperties

	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas isEmpty ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:14:18 am (UTC)'!
test16propertiesReturnsAnNewAdditionalMethodStateWhenNoPropertiesHasBeenSet

	| thisMethod properties |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	properties := thisMethod properties.
	
	self assert: properties isEmpty.
	self assert: properties selector equals: thisContext selector.
	self deny: properties == thisMethod properties ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 11:14:35 am (UTC)'!
test17propertiesReturnsTheAlreadySetAdditionalMethodStateWhenHasProperties

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := thisMethod properties.
	self assert: (properties includesKey: propertyName).
	self assert: properties selector equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 23/Jul/2026, 7:32:00 pm (UTC)'!
test18propertyKeysAndValuesDoIteratesOverProperties

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := OrderedCollection new.
	thisMethod properties propertyKeysAndValuesDo: [ :key :value | properties add: key -> value ].
	
	self assert: properties size equals: 1.
	self assert: properties first equals: propertyName -> true! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'jmv 23/Jul/2026, 7:32:11 pm (UTC)'!
test19propertyKeysAndValuesDoDoesNotEvaluateBlockWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod properties propertyKeysAndValuesDo: [ :key :value | self fail ]! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 8:58:44 pm (UTC)'!
test20propertyValueAtReturnsPropertyValueWhenSetted

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName) equals: propertyValue ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 8:56:20 pm (UTC)'!
test21propertyValueAtReturnsNilWhenPropertyIsNotFound

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: (thisMethod propertyValueAt: #abc) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 8:55:00 pm (UTC)'!
test22propertyValueAtReturnsNilWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 8:58:18 pm (UTC)'!
test23propertyValueAtIfAbsetReturnsPropertyValueWhenFound

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName ifAbsent: [ self fail ]) equals: propertyValue ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 3/Jan/2019, 8:59:33 pm (UTC)'!
test24propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenNotFound

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: #abc ifAbsent: [ propertyValue + 1]) equals: propertyValue + 1 ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:39:17 pm (UTC)'!
test25propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty ifAbsent: [ 1 ]) equals: 1! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:44:05 pm (UTC)'!
test26propertyValueAtPutCreatesPropertiesWithNewOne

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	propertyValue := thisMethod propertyValueAt: testProperty put: 1.
	
	self assert: propertyValue equals: 1.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 1! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:44:40 pm (UTC)'!
test27propertyValueAtPutReplacesPropertyIfAlreadyExisted

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: testProperty put: 1.
	propertyValue := thisMethod propertyValueAt: testProperty put: 2.
	
	self assert: propertyValue equals: 2.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 2! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:45:06 pm (UTC)'!
test28propertyValueAtPutAddsNewPropertyIfPropertiesExisted

	| thisMethod testProperty1 testProperty2 propertyValue |
	
	testProperty1 := #testProperty1.
	testProperty2 := #testProperty2.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: testProperty1 put: 1.
	propertyValue := thisMethod propertyValueAt: testProperty2 put: 2.
	
	self assert: propertyValue equals: 2.
	self assert: (thisMethod propertyValueAt: testProperty1) equals: 1.
	self assert: (thisMethod propertyValueAt: testProperty2) equals: 2! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:47:40 pm (UTC)'!
test29selectorReturnsMethodSelectorWhenNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: thisMethod selector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:48:40 pm (UTC)'!
test30selectorReturnsMethodSelectorWhenHasProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: thisMethod selector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:54:03 pm (UTC)'!
test31SettingSelectorWhenHasNoProperties

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod removeProperties.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 7/Jan/2019, 7:54:47 pm (UTC)'!
test32SettingSelectorWhenHasProperties

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:41:19 pm (UTC)'!
indexOfIv1
	
	^self class indexOfInstanceVariable: 'iv1'! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:44:50 pm (UTC)'!
indexOfIv2
	
	^self class indexOfInstanceVariable: 'iv2'! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:40:38 pm (UTC)'!
iv1

	^iv1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:46:19 pm (UTC)'!
iv1: anObject

	iv1 := anObject ! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:41:59 pm (UTC)'!
iv1_notGetter

	^iv1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:46:58 pm (UTC)'!
iv1_notSetter: anObject

	iv1 := anObject ! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 8:00:45 pm (UTC)'!
iv1ReadWrite

	iv1 := iv1 yourself! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:45:13 pm (UTC)'!
iv2

	^iv2 yourself! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:48:08 pm (UTC)'!
iv2: anObject

	iv2 := 1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:40:33 pm (UTC)'!
test01isGetterWhenSameSelectorAsInstanceVariableAndOnlyReturnsThatVariable

	self assert: (self class>>#iv1 isGetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:42:29 pm (UTC)'!
test02isNotGetterWhenDifferentSelectorAsInstanceVariable

	self deny: (self class>>#iv1_notGetter isGetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:44:13 pm (UTC)'!
test03isNotGetterWhenSameSelectorAsInstanceVariableButDoesOtherThingBesidesReturningVariable

	self deny: (self class>>#iv2 isGetterOf: 'iv2' at: self indexOfIv2)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:46:04 pm (UTC)'!
test04isSetterWhenSameKeywordSelectorAsInstanceVariableAndOnlySetsTheVariable

	self assert: (self class>>#iv1: isSetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:46:52 pm (UTC)'!
test05isNotSetterWhenDifferentKeywordSelectorAsInstanceVariable

	self deny: (self class>>#iv1_notSetter: isSetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:47:57 pm (UTC)'!
test06isNotSetterWhenSameKeywordSelectorAsInstanceVariableButDoesNotAssignParameter

	self deny: (self class>>#iv2: isSetterOf: 'iv2' at: self indexOfIv2)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:58:54 pm (UTC)'!
test07AccessorDescriptionIsGetterForAGetter

	self assert: 'getter' equals: (self class>>#iv1 accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:59:05 pm (UTC)'!
test08AccessorDescriptionIsSetterForASetter

	self assert: 'setter' equals: (self class>>#iv1: accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 7:59:34 pm (UTC)'!
test09AccessorDescriptionIsReadForMethodThatReadsInstanceVariable

	self assert: 'read' equals: (self class>>#iv1_notGetter accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 8:00:19 pm (UTC)'!
test10AccessorDescriptionIsWriteForMethodThatWritesInstanceVariable

	self assert: 'write' equals: (self class>>#iv1_notSetter: accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 9/Apr/2020, 8:01:06 pm (UTC)'!
test11AccessorDescriptionIsWriteReadForMethodThatWritesAndReadInstanceVariable

	self assert: 'write/read' equals: (self class>>#iv1ReadWrite accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!LiteralRefLocatorTest methodsFor: 'tests' stamp: 'eem 24/Jan/2025, 4:31:46 pm (UTC)'!
testFindLiteralsInBytecode
	"Create a method referencing integers, characters, special selectors and nil, true & false.
	 Compile it and check that the objects are found."
	| source binarySpecials integers characters |
	binarySpecials := Smalltalk specialSelectors select: [:s| s isSymbol and: [s isInfix]].
	"-65536 to 65535 in powers of two"
	integers := ((16 to: 1 by: -1) collect: [:power| (2 raisedTo: power) negated]),
				((0 to: 16) collect: [:power| (2 raisedTo: power) - 1]).
	"some printable characters; alas none have code > 255"
	characters := (0 to: 65535)
					select: [:n| (n between: 132 and: 160) not "these have no glyph in typical fonts"
								and: [(Character value: n) shouldBePrintedAsLiteral]]
					thenCollect: [:n| Character value: n].
	[characters size > 32] whileTrue:
		[characters := (1 to: characters size by: 2) collect: [:i| characters at: i]].
	
	#(('' '') ('^[' ']')) do: "And the locators should work whether in a block or not"
		[:pFixes|
		source := String streamContents:
					[:s| | binaries |
					binaries := binarySpecials readStream.
					s nextPutAll: 'exampleMethod'; cr; tab; nextPutAll: pFixes first.
					integers
						do: [:n| s print: n]
						separatedBy:
							[binaries atEnd ifTrue: [binaries reset].
							 s space; nextPutAll: binaries next; space].
					s nextPut: $.; cr; tab.
					s nextPut: ${; space.
					characters
						do: [:c| s print: c]
						separatedBy: [s nextPut: $.; space].
					s space; nextPut: $}; nextPut: $.; cr; tab.
					s nextPutAll: 'true ifTrue: [^nil] ifFalse: [^false]'; nextPutAll: pFixes last].
		BytecodeEncoder subclasses do:
			[:encoderClass| | method |
			method := (Parser new
								encoderClass: encoderClass;
								parse: source class: self class)
							generate.
			binarySpecials, integers, characters, #(nil false true) do: 
				[:literal | self assert: (method hasLiteral: literal)].

			"Now test for false positives..."
			integers, characters, #(nil false true) do:
				[:literal| | simpleSource simpleMethod |
				simpleSource := String streamContents:
									[:s| s nextPutAll: 'exampleMethod'; cr;tab; nextPutAll: pFixes first; print: literal; nextPutAll: ' class'; nextPutAll: pFixes last].
				simpleMethod := (Parser new
										encoderClass: encoderClass;
										parse: simpleSource class: self class)
									generate.
				binarySpecials, integers, characters, #(nil false true) do: [:anyLiteral |
					anyLiteral == literal
						ifTrue: [self assert: (simpleMethod hasLiteral: anyLiteral)]
						ifFalse: [self deny: (simpleMethod hasLiteral: anyLiteral)]]]]]! !

!LiteralRefLocatorTest methodsFor: 'tests' stamp: 'jmv 23/Jul/2026, 1:30:06 pm (UTC)'!
testThoroughFindLiteralsInBytecode
	"Create a method referencing integers, characters, special selectors and nil, true & false.
	 Compile it and check that the objects are found."
	| literals problem |
	literals := #(-1 0 1 $0 $1 1.0 #[1 2 3 4] 'one' #one nil true false NaN).
	problem := Float bindingOf: #NaN.
	BytecodeEncoder subclasses do:
		[:encoderClass| | method |
		#(('' '') ('^[' ']')) do: "And the locators should work whether in a block or not"
			[:pFixes|
			"NaN's binding should still be found even though (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)"
			method := (Parser new
								encoderClass: encoderClass;
								parse: 'foo ', pFixes first, '^NaN', pFixes last class: Float)
							generate.

			"Check our problematic case first."
			 self assert: (method hasLiteral: problem key).
			 "The selector of a method should never be found (unless it occurs as a literal or in a pragma)"
			 self deny: (method hasLiteral: method selector).

			"All the literals should be found in a thorough search, but not otherwise"
			method := (Parser new
								encoderClass: encoderClass;
								parse: 'foo ', pFixes first, '^', literals storeString, pFixes last class: Float)
							generate.
			literals, {problem key} do:
				[:literal | self assert: (method hasLiteral: literal)]].

		"Likewise if in a pragma"
		method := (Parser new
							encoderClass: encoderClass;
							parse: 'foo <pragma: ', literals storeString, ' with: ', problem key storeString, '>' class: Float)
						generate.
		literals, {problem key} do:
			[:literal | self assert: (method hasLiteral: literal)]]! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 2:33:59 pm (UTC)'!
test01FullName

	self assertFullNameOf: #printString arguments: { } is: 'printString'. 
	self assertFullNameOf: #+ arguments: { 'a' } is: '+ a'. 
	self assertFullNameOf: #selector: arguments: { 'a' } is: 'selector: a'. 
	self assertFullNameOf: #selector:argument: arguments: { 'a'. 'b'} is: 'selector: a argument: b'. ! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:25:39 pm (UTC)'!
test02Keywords

	self assertKeywordsOf: #printString arguments: {} is: #(printString).
	self assertKeywordsOf: #+ arguments: {'a'} is: #(+).
	self assertKeywordsOf: #selector: arguments: {#printString} is: #(selector:).
	self assertKeywordsOf: #selector:arguments: arguments: {#printString:. 'b'} is: #(selector: arguments:).
	! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:13:32 pm (UTC)'!
test03HasNoRightNumberOfArgumentsForUnaryMessageAndNotEmptyArguments

	self deny: (Message selector: #printString argument: 'a') hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:14:06 pm (UTC)'!
test04HasRightNumberOfArgumentsForUnaryMessageAndEmptyArguments

	self assert: (Message selector: #printString) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:17:15 pm (UTC)'!
test05HasNoRightNumberOfArgumentsForBinaryMessageAndEmptyArguments

	self deny: (Message selector: #+) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:17:42 pm (UTC)'!
test06HasNoRightNumberOfArgumentsForBinaryMessageAndMoreThanOneArgument

	self deny: (Message selector: #+ arguments: { 'a'. 'b' }) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:18:34 pm (UTC)'!
test07HasRightNumberOfArgumentsForBinaryMessageAndOneArgument

	self assert: (Message selector: #+ argument: 'a') hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:19:17 pm (UTC)'!
test08HasNoRightNumberOfArgumentsForKeywordMessageAndLessArguments

	self deny: (Message selector: #selector:) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:24:12 pm (UTC)'!
test09HasNoRightNumberOfArgumentsForKeywordMessageAndMoreArguments

	self deny: (Message selector: #selector: arguments: { #printString. 'b' }) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 4:24:25 pm (UTC)'!
test10HasRightNumberOfArgumentsForKeywordMessageWhenSameNumberOfKeywordsAndArguments

	self assert: (Message selector: #selector:argument: arguments: { #printString:. 'b' }) hasRightNumberOfArguments! !

!MessageTest methodsFor: 'assertions' stamp: 'HAW 19/Jul/2023, 2:32:37 pm (UTC)'!
assertFullNameOf: aSelector arguments: arguments is: expectedFullName  

	^ self assert: expectedFullName equals: (Message selector: aSelector arguments: arguments ) fullName! !

!MessageTest methodsFor: 'assertions' stamp: 'HAW 19/Jul/2023, 4:04:17 pm (UTC)'!
assertKeywordsOf: aSelector arguments: arguments is: expectedKeyword

	^ self assert: expectedKeyword equals: (Message selector: aSelector arguments: arguments ) keywords! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 18/Dec/2018, 8:27:22 pm (UTC)'!
effectiveZeroExponentForTrigonometry

	^-50! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 15/Dec/2018, 5:19:37 am (UTC)'!
hyperbolicPrecision

	^16! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 15/Dec/2018, 5:12:19 am (UTC)'!
trigonometricDegreePrecision

	^4! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 15/Dec/2018, 4:25:51 am (UTC)'!
trigonometricPrecision

	^3! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 15/Dec/2018, 4:22:44 am (UTC)'!
verify: aFloat is: anInteger floatsAwayFrom: anotherFloat

	self assert: (aFloat floatsAwayFrom: anotherFloat) = anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) = anInteger negated.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) = anInteger negated.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) = anInteger! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 18/Dec/2018, 8:27:33 pm (UTC)'!
verify: aFloat isDegreeTrigonometricallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricDegreePrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 18/Dec/2018, 8:27:38 pm (UTC)'!
verify: aFloat isHyperbolicallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self hyperbolicPrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 18/Dec/2018, 8:27:41 pm (UTC)'!
verify: aFloat isTrigonometricallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricPrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 15/Dec/2018, 4:22:57 am (UTC)'!
verify: aFloat isWithin: anInteger floatsAwayFrom: anotherFloat

	self assert: (aFloat floatsAwayFrom: anotherFloat) abs <= anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) abs <= anInteger.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) abs <= anInteger.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) abs <= anInteger! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'jmv 18/Dec/2018, 2:40:29 pm (UTC)'!
verify: aFloat isWithinOneFloatAwayFrom: anotherFloat

	self verify: aFloat isWithin: 1 floatsAwayFrom: anotherFloat! !

!AbstractNumberTest class methodsFor: 'testing' stamp: 'jmv 27/Jun/2022, 4:17:34 pm (UTC)'!
isAbstract

	^self subclasses notEmpty! !

!FloatTest methodsFor: 'expected failures' stamp: 'jmv 27/Nov/2024, 10:49:57 pm (UTC)'!
expectedFailures
	Smalltalk platformName = 'Mac OS' ifTrue: [
		^#(testE testExp testPrimExpAndLn) ].
	^#().! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 21/Feb/2019, 9:38:32 pm (UTC)'!
testAdd

	self assert: 1.0 + 1.0 = 2.0.
	self assert: Float fminNormalized + Float fminDenormalized > Float fminNormalized.
	self assert: Float fminNormalized + Float fminDenormalized - Float fminNormalized = Float fminDenormalized.
	self assert: 1.0 + Float zero = 1.0.
	self assert: Float zero + 1.0 = 1.0.
	self assert: 1.0 + Float negativeZero = 1.0.
	self assert: Float negativeZero + 1.0 = 1.0.
	self assert: 1.0 + Float infinity = Float infinity.
	self assert: Float infinity + 1.0 = Float infinity.
	self assert: 1.0 + Float negativeInfinity = Float negativeInfinity.
	self assert: Float negativeInfinity + 1.0 = Float negativeInfinity.
	self assert: (1.0 + Float nan) isNaN.
	self assert: (Float nan + 1.0) isNaN.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 2/Jul/2019, 3:56:44 pm (UTC)'!
testDivide

	self assert: 1.5 / 2.0 = 0.75.
	
	self assert: 2.0 / 1 = 2.0.
	
	self should: [ 2.0 / 0 ] raise: ZeroDivide.
	self assert: 2.0 / 0 isExactly: Float infinity.
	self should: [ 2.0 / 0.0 ] raise: ZeroDivide.
	self assert: 2.0 / 0.0 isExactly: Float infinity.
	self should: [ 1.2 / Float negativeZero ] raise: ZeroDivide.
	self assert: 1.2 / Float negativeZero isExactly: Float negativeInfinity.
	self should: [ 1.2 / (1.3 - 1.3) ] raise: ZeroDivide.
	self assert: 1.2 / (1.3 - 1.3) isExactly: Float infinity.

	self assert: 2.0 / 2.0 = 1.0.
	self assert: Float fminNormalized / 2.0 * 2.0 = Float fminNormalized.
	self assert: Float fminDenormalized * 2.0 / 2.0 = Float fminDenormalized.
	self assertIsPositiveZero: Float zero / 1.0.
	self assertIsNegativeZero: Float negativeZero / 1.0.
	self assertIsPositiveZero: 1.0 / Float infinity.
	self assert: Float infinity / 1.0 = Float infinity.
	self assertIsNegativeZero: 1.0 / Float negativeInfinity.
	self assert: Float negativeInfinity / 1.0 = Float negativeInfinity.
	self assert: (1.0 / Float nan) isNaN.
	self assert: (Float nan / 1.0) isNaN.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 21/Feb/2019, 9:53:58 pm (UTC)'!
testMultiply

	self assert: 2.0 * 2.0 = 4.0.
	self assert: Float fminNormalized * 2.0 = (Float fminNormalized + Float fminNormalized).
	self assert: Float fminDenormalized * 2.0 = (Float fminDenormalized + Float fminDenormalized).
	self assertIsPositiveZero: 1.0 * Float zero.
	self assertIsPositiveZero: Float zero * 1.0.
	self assertIsNegativeZero: 1.0 * Float negativeZero.
	self assertIsNegativeZero: Float negativeZero * 1.0.
	self assert: 1.0 * Float infinity = Float infinity.
	self assert: Float infinity * 1.0 = Float infinity.
	self assert: 1.0 * Float negativeInfinity = Float negativeInfinity.
	self assert: Float negativeInfinity * 1.0 = Float negativeInfinity.
	self assert: (1.0 * Float nan) isNaN.
	self assert: (Float nan * 1.0) isNaN.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'dtl 15/Jan/2024, 5:15:00 pm (UTC)'!
testSmallIntegerDivide
	"Mixed integer and float operation should not introduce loss of precision"

	self should: [Compiler evaluate: '(3 / 5.0) = (3.0 / 5.0)']
	! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 21/Feb/2019, 9:41:52 pm (UTC)'!
testSubtract

	self assert: 3.0 - 1.0 = 2.0.
	self assert: Float fminNormalized - Float fminDenormalized < Float fminNormalized.
	self assert: Float fminNormalized - Float fminDenormalized - Float fminNormalized = Float fminDenormalized negated.
	self assert: 1.0 - Float zero = 1.0.
	self assert: Float zero - 1.0 = -1.0.
	self assert: 1.0 - Float negativeZero = 1.0.
	self assert: Float negativeZero - 1.0 = -1.0.
	self assert: 1.0 - Float infinity = Float negativeInfinity.
	self assert: Float infinity - 1.0 = Float infinity.
	self assert: 1.0 - Float negativeInfinity = Float infinity.
	self assert: Float negativeInfinity - 1.0 = Float negativeInfinity.
	self assert: (1.0 - Float nan) isNaN.
	self assert: (Float nan - 1.0) isNaN.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 15/Oct/2019, 8:32:51 pm (UTC)'!
testTimesTwoPowerGradualUnderflow
	"Here is a vicious case where timesTwoPower is inexact because it underflows.
	And two consecutive inexact operations lead to a different result than a single one.
	Typically expressed as multiple of Float fmin in base 2,
	2r1011*Float fmin shifted by -3 with round to nearest, tie to even mode:
	-> round(1.011) -> 1.0 = fmin
	But if first shifted by -2 then by -1:
	-> round(10.11) -> 11.0 = 3*fmin
	-> round(1.1) -> 10.0 = 2*fmin
	Or first shifted by -1 then by -2:
	-> round(101.1) -> 110.0 = 6*fmin
	-> round(1.1) -> 10.0 = 2*fmin
	A naive implementation that split the shift uncarefully might fail to handle such case correctly."

	| f |
	f := 2r1011 asFloat.
	"scan the whole range of possible exponents for this significand"
	Float fmin exponent + f exponent to: Float fmax exponent - f exponent
		do:
			[ :exp |
				| g |
				g := f timesTwoPower: exp.
				(g timesTwoPower: Float fmin exponent - g exponent) = Float fmin ifFalse: [ exp print ].
				"self assert: (g timesTwoPower: Float fmin exponent - g exponent) = Float fmin"
				]

"
testTimesTwoPowerGradualUnderflow
https://github.com/OpenSmalltalk/opensmalltalk-vm/issues/383
"! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 26/Mar/2019, 2:59:17 pm (UTC)'!
testTimesTwoPowerOverflow
	self assert: (Float fminNormalized timesTwoPower: Float emax - Float emin) equals: (2.0 raisedTo: Float emax).
	self assert: (Float zero timesTwoPower: SmallInteger maxVal) equals: Float zero.
	self assert: (Float zero timesTwoPower: SmallInteger maxVal squared) equals: Float zero.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal) equals: Float infinity.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float infinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal) equals: Float negativeInfinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float negativeInfinity.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 26/Mar/2019, 2:58:50 pm (UTC)'!
testTimesTwoPowerUnderflow
	self assert: ((2.0 raisedTo: Float emax) timesTwoPower: Float emin - Float emax) equals: Float fminNormalized.
	self assert: (Float infinity timesTwoPower: SmallInteger minVal * SmallInteger maxVal) equals: Float infinity.
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal squared negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal squared negated).
! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 2/Jul/2019, 4:03:05 pm (UTC)'!
testZeroDividedByZero

	self assert: (0.0 / 0.0) isNaN.
	self assert: (0.0 / -0.0) isNaN.
	self assert: (-0.0 / 0.0) isNaN.
	self assert: (-0.0 / -0.0) isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:56:51 am (UTC)'!
testArCosh
	self assert: 1.0 arCosh = 0.0.
	self deny: Float infinity arCosh isFinite.
	self verify: 2.5 arCosh cosh isHyperbolicallyEqualTo: 2.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 26/Mar/2019, 3:23:12 pm (UTC)'!
testArSinh
	self assertIsPositiveZero: 0.0 arSinh.
	self assertIsNegativeZero: Float negativeZero arSinh.
	self deny: Float infinity arSinh isFinite.
	self verify: 0.5 arSinh negated isHyperbolicallyEqualTo: 0.5 negated arSinh.
	self verify: 0.5 arSinh sinh isHyperbolicallyEqualTo: 0.5.
	self verify: -2.5 arSinh sinh isHyperbolicallyEqualTo: -2.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:57:47 am (UTC)'!
testArTanh
	self assertIsPositiveZero: 0.0 arTanh.
	self assertIsNegativeZero: Float negativeZero arTanh.
	self deny: 1 arTanh isFinite.
	self verify: 0.5 arTanh negated isHyperbolicallyEqualTo: 0.5 negated arTanh.
	self verify: 0.5 arTanh tanh isHyperbolicallyEqualTo: 0.5.
	self verify: -0.5 arTanh tanh isHyperbolicallyEqualTo: -0.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 18/Dec/2018, 2:52:08 pm (UTC)'!
testArcTan

	self verify: (100 arcTan: 100) isTrigonometricallyEqualTo: Float pi / 4.
	self verify: (-100 arcTan: 100) isTrigonometricallyEqualTo: Float pi / -4.
	self verify: (100 arcTan: -100) isTrigonometricallyEqualTo: Float pi * 3 / 4.
	self verify: (-100 arcTan: -100) isTrigonometricallyEqualTo: Float pi * -3 / 4.
	self verify: (0 arcTan: 100) isTrigonometricallyEqualTo: 0.
	self verify: (0 arcTan: -100) isTrigonometricallyEqualTo: Float pi.
	self verify: (100 arcTan: 0) isTrigonometricallyEqualTo: Float pi / 2.
	self verify: (-100 arcTan: 0) isTrigonometricallyEqualTo: Float pi / -2.
	
	self verify: (Float negativeZero arcTan: 100) isTrigonometricallyEqualTo: 0.
	self verify: (Float negativeZero arcTan: -100) isTrigonometricallyEqualTo: Float pi * -1.
	
	self assert: (0 arcTan: 0) = 0.
	self assert: (Float negativeZero arcTan: 0) = 0.
	self verify: (0 arcTan: Float negativeZero) isTrigonometricallyEqualTo: Float pi.
	self verify: (Float negativeZero arcTan: Float negativeZero) isTrigonometricallyEqualTo: Float pi negated.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 9/Dec/2024, 3:29:19 pm (UTC)'!
testArcTanNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 3.0 5.0 20.0] do: [ :x | 
		self assert: (x predecessor arcTanNonPrimitive - x predecessor arcTan) abs < 1.0e-15.
		self assert: (x arcTanNonPrimitive - x arcTan) abs < 1.0e-15.
		self assert: (x successor arcTanNonPrimitive - x successor arcTan) abs < 1.0e-15.
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:48:21 am (UTC)'!
testCos

	self verify: 0.0 cos isTrigonometricallyEqualTo: 1.0.
	self verify: (Float pi / 3) cos isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) cos isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 6) cos isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) cos isTrigonometricallyEqualTo: 0.0! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:58:14 am (UTC)'!
testCosh
	self verify: 0.0 cosh isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh isHyperbolicallyEqualTo: 2.0 negated cosh.
	self deny: Float infinity cosh isFinite	! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 18/Dec/2018, 8:27:57 pm (UTC)'!
testDegreeCos	
	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self verify: 45 degreeCos squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 45.0 degreeCos squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 60 degreeCos isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 60.0 degreeCos isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 120 degreeCos isDegreeTrigonometricallyEqualTo: -0.5.
	self verify: 120.0 degreeCos isDegreeTrigonometricallyEqualTo: -0.5.
	-360 to: 360 by: 1/3 do:
		[:step |
			| expected actual |
			expected := step degreesToRadians cos.
			actual := step degreeCos.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
			expected := step asFloat degreesToRadians cos.
			actual := step asFloat degreeCos.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry]
		].

	"Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [:k |
		self assert: (k*360 + 90) degreeCos = 0.
		self assert: (k*360 - 90) degreeCos = 0.
		self assert: (k*360 + 180) degreeCos + 1 = 0.
		self assert: (k*360) degreeCos - 1 = 0.].! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'nice 4/May/2014, 2:30:50 am (UTC)'!
testDegreeCosForExceptionalValues
	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float negativeInfinity degreeCos isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 18/Dec/2018, 8:28:02 pm (UTC)'!
testDegreeSin	
	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self verify: 45 degreeSin squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 45.0 degreeSin squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 30 degreeSin isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 30.0 degreeSin isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: -30 degreeSin isDegreeTrigonometricallyEqualTo: -0.5.
	self verify: -30.0 degreeSin isDegreeTrigonometricallyEqualTo: -0.5.
	-360 to: 360 by: 1/3 do:
		[:step |
			| expected actual |
			expected := step degreesToRadians sin.
			actual := step degreeSin.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
			expected := step asFloat degreesToRadians sin.
			actual := step asFloat degreeSin.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
		].

	"Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [:k |
		self assert: (k*360 + 90) degreeSin - 1 = 0.
		self assert: (k*360 - 90) degreeSin + 1= 0.
		self assert: (k*360 + 180) degreeSin = 0.
		self assert: (k*360) degreeSin = 0.].! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'nice 4/May/2014, 2:30:55 am (UTC)'!
testDegreeSinForExceptionalValues
	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float negativeInfinity degreeSin isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 27/Nov/2018, 5:15:27 pm (UTC)'!
testExp
	self assert: 0.0 exp = 1.0.
	self assert: 1.0 exp = Float e.
	self assert: 2.0 exp sqrt = Float e! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 27/Nov/2018, 5:28:53 pm (UTC)'!
testExpNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 2.0 5.0 ] do: [ :x | | computed expected |
		computed _ x expNonPrimitive.
		expected _ x exp.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 31/Jul/2023, 3:14:00 pm (UTC)'!
testFloorLog2
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."
	
	| aBitLess aBitMore |
	aBitMore := 1.0 successor.
	aBitLess := 1.0 predecessor.
	Float emin + 1 to: Float emax - 1 do: [:exp |
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: (exactPowerOfTwo floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitMore floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitLess floorLog: 2) equals: exp-1 ].! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 27/Nov/2018, 5:33:48 pm (UTC)'!
testLn
	self assert: 0.0 exp ln = 0.0.
	self assert: 1.0 exp ln = 1.0.
	self assert: 2.0 exp ln = 2.0.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 3:53:24 am (UTC)'!
testLnForExceptionalValues

	self assert: -1.0 ln isNaN.
	self assert: Float infinity ln = Float infinity.
	self assert: Float negativeInfinity ln isNaN! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/Dec/2018, 5:39:49 pm (UTC)'!
testLnNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.01 2.01 5.0 ] do: [ :x | | computed expected |
		computed _ x exp lnNonPrimitive.
		expected _ x exp ln.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 2/Jul/2019, 3:55:02 pm (UTC)'!
testReciprocal

	self 
		assert: 1.0 reciprocal = 1.0;
		assert: 2.0 reciprocal = 0.5;
		assert: -1.0 reciprocal = -1.0;
		assert: -2.0 reciprocal = -0.5.
		
	self should: [ 0.0 reciprocal ] raise: ZeroDivide.
	self assert:  0.0 reciprocal isExactly: Float infinity! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:47:34 am (UTC)'!
testSin

	self verify: 0.0 sin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) sin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) sin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) sin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) sin isTrigonometricallyEqualTo: 1.0! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/Dec/2018, 5:41:55 pm (UTC)'!
testSinNonPrimitive
	self assert: 0.0 sinNonPrimitive = 0.0.
	self assert: (45 degreesToRadians sinNonPrimitive - 0.5 sqrt) abs <= (0.5 ulp * 2).
	self assert: (90 degreesToRadians sinNonPrimitive - 1.0) abs <= (1.0 ulp * 2).! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:58:36 am (UTC)'!
testSinh
	self assertIsPositiveZero: 0.0 sinh.
	self assertIsNegativeZero: Float negativeZero sinh.
	self deny: Float infinity sinh isFinite.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 sinh negated isHyperbolicallyEqualTo: 2.0 negated sinh! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 26/Mar/2019, 8:00:43 pm (UTC)'!
testSqrt
	self assert: Float nan sqrt isNaN.
	self assert: Float infinity sqrt = Float infinity.
	self assertIsPositiveZero: Float zero sqrt.
	self assertIsNegativeZero: Float negativeZero sqrt.
	self assert: Float fminNormalized sqrt squared = Float fminNormalized .
	self assert: Float fminDenormalized sqrt squared = Float fminDenormalized .! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 27/Nov/2018, 3:03:49 pm (UTC)'!
testSqrtNonPrimitive
	self assert: Float nan sqrtNonPrimitive isNaN.
	self assert: Float infinity sqrtNonPrimitive = Float infinity.
	self assertIsPositiveZero: Float zero sqrtNonPrimitive.
	self assertIsNegativeZero: Float negativeZero sqrtNonPrimitive.
	
	self assert: 1.0 sqrt = 1.0 sqrtNonPrimitive.
	self assert: 2.0 sqrt = 2.0 sqrtNonPrimitive.
	self assert: 4.0 sqrt = 4.0 sqrtNonPrimitive.
	self assert: 8.0 sqrt = 8.0 sqrtNonPrimitive.
	self assert: 9.0 sqrt = 9.0 sqrtNonPrimitive.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 15/Dec/2018, 4:58:54 am (UTC)'!
testTanh
	self assertIsPositiveZero: 0.0 tanh.
	self assertIsNegativeZero: Float negativeZero tanh.
	self assert: Float infinity tanh = 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 tanh negated isHyperbolicallyEqualTo: 2.0 negated tanh! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'nice 11/Jul/2009, 2:27:00 am (UTC)'!
testComparisonWhenPrimitiveFails
	"This is related to http://bugs.squeak.org/view.php?id=7361"

	self deny: 0.5 < (1/4).
	self deny: 0.5 < (1/2).
	self assert: 0.5 < (3/4).
	
	self deny: 0.5 <= (1/4).
	self assert: 0.5 <= (1/2).
	self assert: 0.5 <= (3/4).
	
	self assert: 0.5 > (1/4).
	self deny: 0.5 > (1/2).
	self deny: 0.5 > (3/4).
	
	self assert: 0.5 >= (1/4).
	self assert: 0.5 >= (1/2).
	self deny: 0.5 >= (3/4).
	
	self deny: 0.5 = (1/4).
	self assert: 0.5 = (1/2).
	self deny: 0.5 = (3/4).
	
	self assert: 0.5 ~= (1/4).
	self deny: 0.5 ~= (1/2).
	self assert: 0.5 ~= (3/4).! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 8/Oct/2018, 8:20:48 pm (UTC)'!
testComparisonWithFraction
	self assert: 0.5 = (1/2).
	self assert: (1/2) = 0.5.
	self assert: 0.1 = (1/10).
	self assert: (1/10) = 0.1! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 6/Feb/2024, 1:56:06 pm (UTC)'!
testEqualityComparison
	"In Cuis, it was decided to coerce operands to Float for comparison (if any of them was float),
	in the classic ST-80 way"
	| a b c |
	
	"Test equality when Float conversion exact"
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat.
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat asInteger.
	
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) = 1.0.			"Coerce to Float"
	
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) = 1.0.			"Coerce to Float"
	
	"Test exact vs inexact arithmetic"
	(1 to: 100) do: [:i |
		self assert: (1/i) = (1/i asFloat)].
	
	"Test overflow (compare to infinity)"
	a := (11 raisedTo: 400) / 2.
	b := (13 raisedTo: 400) / 2.
	c := a asFloat.
	self assert: ((a = c) & (b = c)).			"Coerce to Float"
	self deny:  (a = b).
	
	"every integer is smaller than infinity"
	self assert: a = Float infinity.			"Coerce to Float"
	self assert: a > Float infinity negated.
	
	"Test underflow"
	self deny: 1 / (11 raisedTo: 400) = 0.
	self assert: 1 / (11 raisedTo: 400) = 0.0.		"Coerce to Float"
	
	"Test hash code"
	self assert:
		((Set newWithRoomForMoreThan: 3) add: 3; add: 3.0; size) =
		((Set newWithRoomForMoreThan: 4) add: 3; add: 3.0; size).


	self assert: 9007199254740996 = 9007199254740996.0.
	self assert: 9007199254740996001 / 1000 = 9007199254740996.0.
	self assert: 90071992547409963 / 10 = 9007199254740996.0.
	self assert: 90071992547409967 / 10 = 9007199254740996.0.
	self assert: 9007199254740995 = 9007199254740996.0.
	self deny: 9007199254740995 = 9007199254740994.0.

	self assert: 9007199254740996.0 = 9007199254740996.
	self assert: 9007199254740996.0 = (9007199254740996001 / 1000).
	self assert: 9007199254740996.0 = (90071992547409963 / 10).
	self assert: 9007199254740996.0 = (90071992547409967 / 10).
	self assert: 9007199254740996.0 = 9007199254740995.
	self deny: 9007199254740994.0 = 9007199254740995.! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 4/Dec/2018, 4:05:21 pm (UTC)'!
testEquals
	self assert: 1.0 = 1.0.
	self deny: 1.0 = 2.0.
	self assert: 0.0 = 0.0.
	self deny: 0.0 = 1.0.
	self deny: 1.0 = 0.0.
	self assert: 1.0 = 1.
	self deny: 1.0 = 2.
	self assert: Float zero = Float negativeZero.
	self assert: Float negativeZero = Float zero.
	self assert: Float fmax = Float fmax.
	self assert: Float fminNormalized = Float fminNormalized.
	self assert: Float fminDenormalized = Float fminDenormalized.
	self deny: Float fmax = Float fminNormalized.
	self deny: Float fminNormalized = Float fmax.
	self deny: Float fminDenormalized = Float fminNormalized.
	self deny: Float fminNormalized = Float fminDenormalized.! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'sqr 15/Dec/2018, 3:02:34 am (UTC)'!
testFloatsAwayFrom

	self verify: 1.0 is: 0 floatsAwayFrom: 1.0.
	self verify: 1.0 is: -1 floatsAwayFrom: 1.0 successor.
	self verify: 1.0 is: 1 floatsAwayFrom: 1.0 predecessor.
	self verify: 0.0 is: 0 floatsAwayFrom: Float negativeZero.
	self verify: Float infinity is: 1 floatsAwayFrom: Float fmax.
	self verify: Float fmax is: -1 floatsAwayFrom: Float infinity.
	self verify: Float negativeInfinity is: -1 floatsAwayFrom: Float fmax negated.
	self verify: Float fmax negated is: 1 floatsAwayFrom: Float negativeInfinity.
	self verify: 0.0 is: -1 floatsAwayFrom: Float fminDenormalized.
	self verify: Float negativeZero is: -1 floatsAwayFrom: Float fminDenormalized.
	self verify: Float fmax is: (1 bitShift: Float precision - 1) - 1 floatsAwayFrom: (1.0 timesTwoPower: Float emax)! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'sqr 15/Dec/2018, 3:02:43 am (UTC)'!
testFloatsAwayFromWithPowersOfTwo

	| previous current soMany |
	previous := 0.0.
	current := Float fminDenormalized.
	soMany := 1.
	[current isFinite] whileTrue:
		[
			self verify: current is: soMany floatsAwayFrom: previous.
			soMany < 4503599627370496 :: and: [previous > 0.0] :: ifTrue: [soMany := soMany * 2].
			previous := current.
			current := current * 2.0
		]! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 8/Oct/2018, 8:18:13 pm (UTC)'!
testHugeValues

	self assert: 1e400 = Float infinity.
	self deny: 1e400 < Float infinity.
	self deny: 1e400 > Float infinity.

	self assert: 1e400 = 1.0e309.
	self assert: 1e400 > 1.0e308.


	self assert: -1e400 = Float negativeInfinity.
	self deny: -1e400 < Float negativeInfinity.
	self deny: -1e400 > Float negativeInfinity.

	self assert: -1e400 = -1.0e309.
	self assert: -1e400 < -1.0e308.! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'nice 28/Oct/2014, 1:50:00 am (UTC)'!
testLiteralEqualityOfNan
	| nan |
	nan := Float nan.
	self assert: (nan literalEqual: nan)
		description: 'Float nan is not equal to itself, though it is literally equal'.! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 3/Oct/2018, 8:29:31 pm (UTC)'!
testLiteralEqualityOfZeroAndNegativeZero
	self assert: Float zero = Float negativeZero.
	self deny: (Float zero literalEqual: Float negativeZero).
	self
		assert: (Compiler evaluate: '1>2 ifTrue: [0.0] ifFalse: [-0.0]') hex = Float negativeZero hex
		description: 'Float zero and negativeZero are not literally substituable'.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 27/Mar/2019, 7:10:58 pm (UTC)'!
testAsTrueFraction
	"
	self new testAsTrueFraction
	"
	| x |
	x _ Float pi.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.0 / 3.0.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fminNormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fminNormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fminDenormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fminDenormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.023399999997e-312.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fmax.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fmax.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.0234e308.
	self assert: x asTrueFraction asFloat = x.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/Dec/2018, 4:06:58 pm (UTC)'!
testCeiling
	self assert: 1.0 ceiling = 1.
	self assert: 1.0 successor ceiling = 2.
	self assert: 1.0 predecessor ceiling = 1.
	self assert: 1.1 ceiling = 2.
	self assert: -2.0 ceiling = -2.
	self assert: -2.1 ceiling = -2.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 24/Jul/2008, 6:04:00 am (UTC)'!
testFloatRounded
	"5000000000000001 asFloat has an exact representation (no round off error).
	It should round to nearest integer without loosing bits.
	This is a no regression test on http://bugs.squeak.org/view.php?id=7134"
	
	| x y int r |
	
	"This is a preamble asserting exactness of representation
	and quality of various conversions"
	int := 5000000000000001.
	x := int asFloat.
	y := (5 asFloat squared squared squared squared timesTwoPower: 15) + 1.
	self assert: x = y.
	self assert: x asTrueFraction = int.
	
	"this one should be true for any float
	in order to conform to ISO/IEC 10967-2"
	self assert: x rounded = x asTrueFraction rounded.
	self assert: x negated rounded = x negated asTrueFraction rounded.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x rounded = x asTrueFraction rounded.
		self assert: x negated rounded = x negated asTrueFraction rounded]! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 26/Apr/2006, 9:21:00 am (UTC)'!
testFloatTruncated
	"(10 raisedTo: 16) asFloat has an exact representation (no round off error).
	It should convert back to integer without loosing bits.
	This is a no regression test on http://bugs.impara.de/view.php?id=3504"
	
	| x y int r |
	int := 10 raisedTo: 16.
	x := int asFloat.
	y := (5 raisedTo: 16) asFloat timesTwoPower: 16.
	self assert: x = y.
	
	self assert: x asInteger = int.
	
	"this one should be true for any float"
	self assert: x asInteger = x asTrueFraction asInteger.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x truncated = x asTrueFraction truncated]! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/Dec/2018, 4:07:13 pm (UTC)'!
testFloor
	self assert: 1.0 floor = 1.
	self assert: 1.0 successor floor = 1.
	self assert: 1.0 predecessor floor = 0.
	self assert: 1.1 floor = 1.
	self assert: -2.0 floor = -2.
	self assert: -2.1 floor = -3.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 28/Mar/2016, 1:48:00 pm (UTC)'!
testFractionAsFloat
	"use a random test"
	
	| r m frac err collec |
	r := Random new seed: 1234567.
	m := (2 raisedTo: 54) - 1.
	200 timesRepeat: [
		frac := ((r nextInteger: m) * (r nextInteger: m) + 1) / ((r nextInteger: m) * (r nextInteger: m) + 1).
		err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
		self assert: err < (1/2)].
	
	collec := #(16r10000000000000 16r1FFFFFFFFFFFFF 1 2 16r20000000000000 16r20000000000001 16r3FFFFFFFFFFFFF 16r3FFFFFFFFFFFFE 16r3FFFFFFFFFFFFD).
	collec do: [:num |
		collec do: [:den |
			frac := Fraction numerator: num denominator: den.
			err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
			self assert: err <= (1/2)]].! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 10/Jan/2007, 6:29:00 am (UTC)'!
testFractionAsFloat2
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction = ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction = ((1<<52)+2).! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/Oct/2018, 8:47:20 pm (UTC)'!
testFractionAsFloatRoundsHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| floatWithEvenMantissa floatWithEvenMantissa2 floatWithOddMantissa |
	floatWithEvenMantissa _ Float maxExactInteger * 1.23.
	self assert: floatWithEvenMantissa mantissaPart even description: 'precondition'.

	floatWithOddMantissa _ floatWithEvenMantissa successor.
	self assert: floatWithOddMantissa mantissaPart even not description: 'precondition'.

	floatWithEvenMantissa2 _ floatWithOddMantissa successor.
	self assert: floatWithEvenMantissa2 mantissaPart even description: 'precondition'.

	self assert: ((floatWithEvenMantissa asTrueFraction *4) + (floatWithOddMantissa asTrueFraction *0) / 4) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *3) + (floatWithOddMantissa asTrueFraction *1) / 4) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *100) + (floatWithOddMantissa asTrueFraction *1) / 101) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *1) + (floatWithOddMantissa asTrueFraction *100) / 101) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *1) + (floatWithOddMantissa asTrueFraction *3) / 4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *0) + (floatWithOddMantissa asTrueFraction *4) / 4) = floatWithOddMantissa description: 'precondition'.

	self assert: ((floatWithEvenMantissa asTrueFraction *2) + (floatWithOddMantissa asTrueFraction *2) / 4) = floatWithEvenMantissa description: 'Must round to closest even mantissa'.

	self assert: ((floatWithOddMantissa asTrueFraction *4) + (floatWithEvenMantissa2 asTrueFraction *0)/4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *3) + (floatWithEvenMantissa2 asTrueFraction *1)/4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *100) + (floatWithEvenMantissa2 asTrueFraction *1)/101) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *1) + (floatWithEvenMantissa2 asTrueFraction *100)/101) = floatWithEvenMantissa2 description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *1) + (floatWithEvenMantissa2 asTrueFraction *3)/4) = floatWithEvenMantissa2 description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *0) + (floatWithEvenMantissa2 asTrueFraction *4)/4) = floatWithEvenMantissa2 description: 'precondition'.

	self assert: ((floatWithOddMantissa asTrueFraction *2) + (floatWithEvenMantissa2 asTrueFraction *2)/4) = floatWithEvenMantissa2 description: 'Must round to closest even mantissa'.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 3/Oct/2018, 8:22:06 pm (UTC)'!
testFractionAsFloatWithUnderflow
	"test rounding to nearest even"

	| underflowPower |
	underflowPower := Float emin - Float precision.
	self assertIsPositiveZero: (2 raisedTo: underflowPower) asFloat.	
	self assertIsNegativeZero: (2 raisedTo: underflowPower) negated asFloat! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 7/May/2006, 2:13:00 am (UTC)'!
testIntegerAsFloat
	"assert IEEE 754 round to nearest even mode is honoured"
	
	self deny: 16r1FFFFFFFFFFFF0801 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 65 bits"
	self deny: 16r1FFFFFFFFFFFF0802 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 64 bits"
	self assert: 16r1FFFFFFFFFFF1F800 asFloat = 16r1FFFFFFFFFFF20000 asFloat. "nearest even is upper"
	self assert: 16r1FFFFFFFFFFFF0800 asFloat = 16r1FFFFFFFFFFFF0000 asFloat. "nearest even is lower"
! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 24/Apr/2019, 3:56:51 pm (UTC)'!
testMixedTypeArithmetic

	| samples doubleOne |

	samples := Array with: 1 with: 1 / 3 with: Float pi with: SmallInteger maxVal * 2 + 1 with: SmallInteger minVal * 2 - 1.
	doubleOne := 1.0.
	samples do: [ :sample |
		self assert: sample + doubleOne isExactly: sample asFloat + doubleOne.
		self assert: doubleOne + sample isExactly: doubleOne + sample asFloat.
		self assert: sample - doubleOne isExactly: sample asFloat - doubleOne.
		self assert: doubleOne - sample isExactly: doubleOne - sample asFloat.
		self assert: sample * doubleOne isExactly: sample asFloat * doubleOne.
		self assert: doubleOne * sample isExactly: doubleOne * sample asFloat.
		self assert: sample / doubleOne isExactly: sample asFloat / doubleOne.
		self assert: doubleOne / sample isExactly: doubleOne / sample asFloat.
		self assert: sample \\ doubleOne isExactly: sample asFloat \\ doubleOne.
		self assert: doubleOne \\ sample isExactly: doubleOne \\ sample asFloat.
		self assert: (sample // doubleOne) = (sample asFloat // doubleOne).
		self assert: (doubleOne // sample) = (doubleOne // sample asFloat).
		].! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 24/Apr/2019, 4:01:05 pm (UTC)'!
testMixedTypeComparison

	| samples doubleOne |

	samples := Array with: 1 with: 1 / 3 with: Float pi with: SmallInteger maxVal * 2 + 1 with: SmallInteger minVal * 2 - 1.
	doubleOne := 1.0.
	samples do: [ :sample |
		self assert: (sample = doubleOne) = (sample asFloat = doubleOne).
		self assert: (doubleOne = sample) = (doubleOne = sample asFloat).
		self assert: (sample ~= doubleOne) = (sample asFloat ~= doubleOne).
		self assert: (doubleOne ~= sample) = (doubleOne ~= sample asFloat).
		self assert: (sample < doubleOne) = (sample asFloat < doubleOne).
		self assert: (doubleOne < sample) = (doubleOne < sample asFloat).
		self assert: (sample <= doubleOne) = (sample asFloat <= doubleOne).
		self assert: (doubleOne <= sample) = (doubleOne <= sample asFloat).
		self assert: (sample > doubleOne) = (sample asFloat > doubleOne).
		self assert: (doubleOne > sample) = (doubleOne > sample asFloat).
		self assert: (sample >= doubleOne) = (sample asFloat >= doubleOne).
		self assert: (doubleOne >= sample) = (doubleOne >= sample asFloat).
		].! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 29/Oct/2021, 3:12:28 pm (UTC)'!
testRoundHalfAwayFromZero
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero"
	self assert: 0.5 roundedHAFZ = 1.
	self assert: 1.5 roundedHAFZ = 2.
	self assert: -0.5 roundedHAFZ = -1.
	self assert: -1.5 roundedHAFZ = -2.
	self assert: (((0.0 to: 5.0 by: 0.25) collect: [ :f | f roundedHAFZ ]) = #(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5) ).
	self assert: (((-0.0 to: -5.0 by: -0.25) collect: [ :f | f roundedHAFZ ]) = #(0 0 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3 -3 -3 -4 -4 -4 -4 -5 -5 -5) )! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 9/Oct/2018, 8:12:35 pm (UTC)'!
testRoundHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: 0.5 rounded = 0.
	self assert: 1.5 rounded = 2.
	self assert: -0.5 rounded = -0.
	self assert: -1.5 rounded = -2.
	self assert: (((0.0 to: 5.0 by: 0.25) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((-0.0 to: -5.0 by: -0.25) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) )! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 4/Jun/2011, 1:37:00 am (UTC)'!
testRounded
	self assert: 0.9 rounded = 1.
	self assert: 1.0 rounded = 1.
	self assert: 1.1 rounded = 1.
	self assert: -1.9 rounded = -2.
	self assert: -2.0 rounded = -2.
	self assert: -2.1 rounded = -2.
	
	"In case of tie, round to upper magnitude"
	self assert: 1.5 rounded = 2.
	self assert: -1.5 rounded = -2.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/Oct/2018, 8:53:57 pm (UTC)'!
testStringAsFloatRoundsHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| floatWithEvenMantissa floatWithEvenMantissa2 floatWithOddMantissa halfway1 halfway2 |
	floatWithEvenMantissa _ Float maxExactInteger * 1.23.
	self assert: floatWithEvenMantissa mantissaPart even description: 'precondition'.

	floatWithOddMantissa _ floatWithEvenMantissa successor.
	self assert: floatWithOddMantissa mantissaPart even not description: 'precondition'.

	floatWithEvenMantissa2 _ floatWithOddMantissa successor.
	self assert: floatWithEvenMantissa2 mantissaPart even description: 'precondition'.

	halfway1 _ floatWithEvenMantissa asTrueFraction + floatWithOddMantissa asTrueFraction / 2.
	self assert: halfway1 = floatWithEvenMantissa description: 'precondition-Must round to closest even mantissa'.
	self assert: halfway1 printString = '11078855083331421' description: 'precondition'.
	self assert: '11078855083331421.0' asNumber = floatWithEvenMantissa description: 'when converting string to Float, if tie, round to closest even mantissa'.

	halfway2 _ floatWithOddMantissa asTrueFraction + floatWithEvenMantissa2 asTrueFraction / 2.
	self assert: halfway2 = floatWithEvenMantissa2 description: 'precondition-Must round to closest even mantissa'.
	self assert: halfway2 printString = '11078855083331423' description: 'precondition'.
	self assert: '11078855083331423.0' asNumber = floatWithEvenMantissa2 description: 'when converting string to Float, if tie, round to closest even mantissa'.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/Oct/2018, 9:17:33 pm (UTC)'!
testStringAsFloatRoundsHalfToEven2
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| evenMantissa oddMantissa scaledEven scaledOdd evenMantissa2 scaledEven2 |
	evenMantissa _ 1.0.
	self assert: evenMantissa mantissaPart even description: 'precondition'.
	oddMantissa _ 1.0 successor.
	self assert: oddMantissa mantissaPart even not description: 'precondition'.
	evenMantissa2 _ oddMantissa successor.
	self assert: evenMantissa2 mantissaPart even description: 'precondition'.

	scaledEven _ evenMantissa asTrueFraction * 1e53.
	scaledOdd _ oddMantissa asTrueFraction * 1e53.
	scaledEven2 _ evenMantissa2 asTrueFraction * 1e53.

	self assert: (scaledEven + scaledOdd / 2) printString = '100000000000000011102230246251565404236316680908203125' description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203124999' asNumber = evenMantissa description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203125001' asNumber = oddMantissa description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203125' asNumber = evenMantissa description: 'when converting string to Float, if tie, round to closest even mantissa'.

	self assert: (scaledEven2 + scaledOdd / 2) printString = '100000000000000033306690738754696212708950042724609375' description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609374999' asNumber = oddMantissa description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609375001' asNumber = evenMantissa2 description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609375' asNumber = evenMantissa2 description: 'when converting string to Float, if tie, round to closest even mantissa'.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/Dec/2018, 4:06:36 pm (UTC)'!
testTruncated
	self assert: 1.0 truncated = 1.
	self assert: 1.0 successor truncated = 1.
	self assert: 1.0 predecessor truncated = 0.
	self assert: 1.1 truncated = 1.
	self assert: -2.0 truncated = -2.
	self assert: -2.1 truncated = -2.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 21/Dec/2018, 3:36:52 pm (UTC)'!
testExactAsString
	"Ensures round-trip string conversion when using #asString.
	Use some relevant examples."

	| examples |
	examples _ {
		1.0. 
		Float fminNormalized / 2. 
		Float fminNormalized / 10. 
		Float fminNormalized / 13. 
		2.0. 
		2 sqrt. 
		Float pi. 
		Float fminDenormalized. 
		Float fminDenormalized * 2. 
		Float fminDenormalized * 3. 
		Float fminDenormalized * 13 },
	{
		Float zero.
		Float negativeZero.
		Float negativeInfinity.
		Float infinity.
		Float fmax predecessor }.

	examples do: [ :float |
		self assert: float predecessor asString asNumber = float predecessor.
		self assert: float asString asNumber = float.
		self assert: float successor asString asNumber = float successor ].

	"NaN are special, in that they are not even equal to themselves"
	self assert: Float nan asString asNumber isNaN! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 21/Dec/2018, 6:56:27 pm (UTC)'!
testExactAsString2
	"Ensures round-trip string conversion when using #asString.
	Use some relevant examples.
	Note: there is no guarantee to restore the bit pattern of NaN though"

	self assert: Float halfPi asString asNumber = Float halfPi.
	self assert: Float halfPi negated asString asNumber = Float halfPi negated.
	self assert: Float pi asString asNumber = Float pi.
	self assert: Float pi negated asString asNumber = Float pi negated.
	self assert: 2.0 sqrt asString asNumber = 2.0 sqrt.
	self assert: 2.0 sqrt negated asString asNumber = 2.0 sqrt negated.
	self assert: Float infinity asString asNumber = Float infinity.
	self assert: Float negativeInfinity asString asNumber = Float negativeInfinity.
	self assert: Float nan asString asNumber isNaN.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 24/Sep/2018, 2:51:07 pm (UTC)'!
testExactStoreString
	"Tests that conversion to / from Strings is exact (same Float is re-created) when using #storeString
	(whose output is meant to be compilable smalltalk code).
	Use some relevant examples."

	| examples |
	examples _ {
		1.0. 
		Float fminNormalized / 2. 
		Float fminNormalized / 10. 
		Float fminNormalized / 13. 
		2.0. 
		2 sqrt. 
		Float pi. 
		Float fminDenormalized. 
		Float fminDenormalized * 2. 
		Float fminDenormalized * 3. 
		Float fminDenormalized * 13 },
	{
		Float zero.
		Float negativeZero.
		Float negativeInfinity.
		Float infinity }.

	examples do: [ :float |
		self assert: (Compiler evaluate: float predecessor storeString) = float predecessor.
		self assert: (Compiler evaluate: float storeString) = float.
		self assert: (Compiler evaluate: float successor storeString) = float successor ].

	"NaN are special, in that they are not even equal to themselves"
	self assert: (Compiler evaluate: Float nan storeString) isNaN! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 4/Dec/2018, 4:09:53 pm (UTC)'!
testExactStoreString2
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 21/Dec/2018, 3:40:15 pm (UTC)'!
testFromStringDoesNotUnderflow
	self assert: '12345678901234567890.0e-330' asNumber isZero not.
	self assert: (Compiler evaluate: '12345678901234567890.0e-330')isZero not.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 21/Dec/2018, 3:44:33 pm (UTC)'!
testMinimalAsString
	"Tests that the conversion from String / to String ends in a minimal String representation.
	This means that there are no extra unneded digits at the end.
	For example, 0.1 (actually the closest Float to 1/10, i.e. ((1/10) asFloat), prints as 0.1, because 
	it can be recovered from that String"

	"These are just to start on solid ground. If these fail, what follows is meaningless."
	self assert: 0.1 = ((1/10) asFloat).		"0.1 really is what it should be. Compiler works ok."
	self assert: '0.1' asNumber = 0.1.			"#asNumber works ok. The string '0.1' is what we want."
	"Now the real test."
	self assert: 0.1 printString = '0.1'.		"String '0.1' is what we get. No unneded extra digits."

	"Whatever Float these Strings represent, print them back the same (no extra digits)"
	self assert: '0.2' asNumber printString = '0.2'.
	#(
		'0.1' '0.2' '0.3' '0.4' '0.5' '0.6' '0.7' '0.8' '0.9' 
		'1.0' '1.1' '1.2' '1.3' '1.4' '1.5' '1.6' '1.7' '1.8' '1.9' '2.0'
		'1.01' '1.001' '1.0001' '1.00001' '1.000001' '1.0000001' '1.00000001' '1.000000001' '1.0000000001' '1.00000000001' '1.000000000001'
		'1.03' '1.003' '1.0003' '1.00003' '1.000003' '1.0000003' '1.00000003' '1.000000003' '1.0000000003' '1.00000000003' '1.000000000003' 
	) do: [ :string |
		self assert: string asNumber printString = string ].! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 21/Dec/2018, 4:15:04 pm (UTC)'!
testPrintShowingDecimalPlaces4
	| requiredDigits |
	requiredDigits _ Float pi printString size - 2.
	self assert: (Float pi printStringFractionDigits: requiredDigits) = Float pi printString.
	0 to: 100 do: [ :extra |
		self assert: (Float pi printStringFractionDigits: requiredDigits + extra) asNumber = Float pi ]! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 1/Mar/2025, 9:22:39 pm (UTC)'!
testPrintStringStress
	| eWidth mWidth ePrimeScale mPrimeScale |
	eWidth := Float emax - Float emin.
	"211 for a more exhaustive but slower test. 41 for a quick run."
	ePrimeScale := 211.
	ePrimeScale := 41.
	mWidth := 1 bitShift: Float precision.
	"9931 for a more exhaustive but slower test. 37 for a quick run."
	mPrimeScale := 9931.
	mPrimeScale := 37.
	0
		to: mWidth - 1 * mPrimeScale
		by: mWidth
		do: [ :mScaled | | m eAdjustment |
			m := mScaled // mPrimeScale.
			eAdjustment := m highBit - 1.
			"{mScaled. m. m hex. eAdjustment } print."
			Float emin * ePrimeScale
				to: Float emax * ePrimeScale
				by: eWidth
				do: [ :eFraction | | e fp |
					e := eFraction // ePrimeScale - eAdjustment.
					fp := m asFloat timesTwoPower: e.
					self assert: fp printString asNumber = fp.
					self assert: fp negated printString asNumber = fp negated ]]! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'nice 15/Mar/2008, 3:59:00 am (UTC)'!
testReadFromManyDigits
	"A naive algorithm may interpret these representations as Infinity or NaN.
	This is http://bugs.squeak.org/view.php?id=6982"
	
	| s1 s2 |
	s1 := '1' , (String new: 321 withAll: $0) , '.0e-321'.
	s2 := '0.' , (String new: 320 withAll: $0) , '1e321'.
	self assert: (Number readFrom: s1) = 1.
	self assert: (Number readFrom: s2) = 1.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 19/Oct/2018, 8:33:22 pm (UTC)'!
testSomeNines
	| twelveNines |
	twelveNines _ Compiler evaluate: '999999 + 0.999999'.
	self assert: twelveNines printString = '999999.999999'.
	self assert: twelveNines printString asNumber = twelveNines.
	self assert: twelveNines printString asNumber - twelveNines = 0.0.
	self assert: twelveNines + 1e-6 = 1e6! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'nice 12/Oct/2008, 1:45:00 am (UTC)'!
testStoreBase16
	"This bug was reported in mantis http://bugs.squeak.org/view.php?id=6695"

	self
		assert: (20.0 storeStringBase: 16) = '16r14.0'
		description: 'the radix prefix should not be omitted, except in base 10'! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 4/Dec/2018, 4:09:57 pm (UTC)'!
testStoreOn
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'dtl 18/Sep/2004, 4:40:00 pm (UTC)'!
testStringAsNumber
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '10r-12.3456' asNumber.
	self assert: -12.3456 = aFloat.
	aFloat := '10r-12.3456e2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456d2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '12.3456q2' asNumber.
	self assert: 1234.56 = aFloat.
! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/Dec/2018, 6:44:07 pm (UTC)'!
testStringAsNumber2
	"Check that small but normal Floats can be created from Strings"

	| aFloat |
	aFloat _ '12345678901234567890.0e-326' asNumber.
	self deny: aFloat = 0.0.
	self assert: aFloat > 0.0.
	self assert: aFloat - 1.2345678901234568e-307 = 0.
	self assert: aFloat = 1.2345678901234568e-307.! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 6:02:56 pm (UTC)'!
testHashWithBigNegativeSmallInteger
	"Not in the Float range"
	| a float smallInteger |
	smallInteger _ SmallInteger minVal+1.
	self deny: smallInteger isLarge description: 'precondition'.
	float _ smallInteger asFloat.
	self assert: smallInteger = float description: 'precondition'.
	self assert: float = smallInteger description: 'precondition'.
	a _ Set new.
	a add: smallInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 6:01:27 pm (UTC)'!
testHashWithBigSmallInteger
	"Not in the Float range"
	| a float smallInteger |
	smallInteger _ SmallInteger maxVal.
	self deny: smallInteger isLarge description: 'precondition'.
	float _ smallInteger asFloat.
	self assert: smallInteger = float description: 'precondition'.
	self assert: float = smallInteger description: 'precondition'.
	a _ Set new.
	a add: smallInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 2:43:35 pm (UTC)'!
testHashWithFraction

	| a float fraction |
	fraction _ 1/3.
	float _ (1/3) asFloat.
	self assert: fraction = float description: 'precondition'.
	self assert: float = fraction description: 'precondition'.
	a _ Set new.
	a add: fraction; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 2:51:32 pm (UTC)'!
testHashWithLargeNegativeInteger
	"Not in the Float range"
	| a float largeInteger |
	largeInteger _ -1e400.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 2:51:37 pm (UTC)'!
testHashWithLargePositiveInteger
	"Not in the Float range"
	| a float largeInteger |
	largeInteger _ 1e400.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 2:51:40 pm (UTC)'!
testHashWithSmallishLargeNegativeInteger
	"In the Float range"
	| a float largeInteger |
	largeInteger _ SmallInteger minVal -1.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/Oct/2018, 7:40:00 pm (UTC)'!
testHashWithSmallishLargeNegativeInteger2
	"In the Float range"
	| a float integer |
	integer _ Float maxExactInteger negated -1.
	float _ integer asFloat.
	self assert: integer = float description: 'precondition'.
	self assert: float = integer description: 'precondition'.
	a _ Set new.
	a add: integer; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 9/Oct/2018, 2:51:44 pm (UTC)'!
testHashWithSmallishLargePositiveInteger
	"In the Float range"
	| a float largeInteger |
	largeInteger _ SmallInteger maxVal +1.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/Oct/2018, 7:40:11 pm (UTC)'!
testHashWithSmallishLargePositiveInteger2
	"In the Float range"
	| a float integer |
	integer _ Float maxExactInteger +1.
	float _ integer asFloat.
	self assert: integer = float description: 'precondition'.
	self assert: float = integer description: 'precondition'.
	a _ Set new.
	a add: integer; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/Oct/2018, 7:57:54 pm (UTC)'!
testNoIntegerHash

	self deny: Float pi hash = 3 hash.
	self deny: Float pi hash = (Float pi *1.00000000000001) hash.
	self deny: Float pi hash = (Float pi /1.00000000000001) hash.! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 6/Feb/2024, 1:56:29 pm (UTC)'!
testSetOfFloat
	"Classical disagreement between hash and = did lead to a bug.
	This is a non regression test from http://bugs.squeak.org/view.php?id=3360"

	| size3 size4 |
	size3 := (Set newWithRoomForMoreThan: 3) add: 3; add: 3.0; size.
	size4 := (Set newWithRoomForMoreThan: 4) add: 3; add: 3.0; size.
	self assert: size3 = size4 description: 'The size of a Set should not depend on its capacity.'! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 27/Dec/2018, 10:37:19 pm (UTC)'!
testE
	"Just in case..."

	self assert: Float e = 2.718281828459045.
	self assert: Float e ln = 1.0.
	self assert: Float e = 1.0 exp.
	self assert: Float e hex = '4005BF0A8B145769'.! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 11/Oct/2011, 12:55:00 pm (UTC)'!
testMaxExactInteger
	"
	FloatTest new testMaxExactInteger
	"

	self assert: Float maxExactInteger asFloat truncated = Float maxExactInteger.
	0 to: 10000 do: [ :j |
		self assert: (Float maxExactInteger-j) asFloat truncated = (Float maxExactInteger-j) ].
	self deny: (Float maxExactInteger+1) asFloat truncated = (Float maxExactInteger+1)
	! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 27/Dec/2018, 10:39:24 pm (UTC)'!
testPi
	"Just in case..."

	self assert: Float pi = 3.141592653589793.
	self assert: Float pi = (1.0 arcTan * 4).
	self assert: Float pi hex = '400921FB54442D18' ! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 5/Apr/2019, 8:23:22 pm (UTC)'!
testCharacterization

	"Test the largest finite representable floating point value"
	self assert: Float fmax successor = Float infinity.
	self assert: Float infinity predecessor = Float fmax.
	self assert: Float fmax negated predecessor = Float infinity negated.
	self assert: Float infinity negated successor = Float fmax negated.
	
	"Test the smallest positive representable floating point value"
	self assertIsPositiveZero: Float fmin predecessor.
	self assert: 0.0 successor = Float fmin.
	self assert: Float fmin negated successor hex = -0.0 hex.
	self assert: -0.0 predecessor = Float fmin negated.
	
	"Test the relative precision"
	self assert: Float one + Float epsilon > Float one.
	self assert: Float one + Float epsilon = Float one successor.
	self assert: Float one + (Float epsilon / Float radix) = Float one.
	
	"Test maximum and minimum exponent"
	self assert: Float fmax exponent = Float emax.
	self assert: Float fminNormalized exponent = Float emin.
	Float denormalized ifTrue: [
		self assert: Float fminDenormalized exponent = (Float emin + 1 - Float precision)].
	
	"Alternative tests for maximum and minimum exponents and normalized and denormal values"
	self assert: (Float radix raisedTo: Float emax) *  (Float radix - (Float epsilon)) = Float fmax.
	self assert: (Float radix raisedTo: Float emin) = Float fminNormalized.
	self assert: (Float radix raisedTo: Float emin) * Float epsilon = Float fmin.
	
	"Test sucessors and predecessors"
	self assert: Float one predecessor successor = Float one.
	self assert: Float one successor predecessor = Float one.
	self assert: Float one negated predecessor successor = Float one negated.
	self assert: Float one negated successor predecessor = Float one negated.
	self assert: Float infinity successor = Float infinity.
	self assert: Float negativeInfinity predecessor = Float negativeInfinity.
	self assertIsNegativeZero: Float fmin negated successor.
	self assertIsPositiveZero: Float fmin predecessor.
	self assert: Float nan predecessor isNaN.
	self assert: Float nan successor isNaN.
	
	"SPECIFIC FOR IEEE 754 double precision - 64 bits"
	self assert: Float fmax hex = '7FEFFFFFFFFFFFFF'.
	self assert: Float fminDenormalized hex = '0000000000000001'.
	self assert: Float fminNormalized hex = '0010000000000000'.
	self assert: 0.0 hex = '0000000000000000'.
	self assert: Float negativeZero hex = '8000000000000000'.
	self assert: Float one hex = '3FF0000000000000'.
	self assert: Float infinity hex = '7FF0000000000000'.
	self assert: Float negativeInfinity hex = 'FFF0000000000000'.! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 4/Dec/2018, 7:20:59 pm (UTC)'!
testNextAwayFromZero

	self assert: Float pi nextAwayFromZero = Float pi successor.
	self assert: Float pi negated nextAwayFromZero = Float pi negated predecessor.
	self assert: 1.0 nextAwayFromZero = 1.0 successor.
	self should: [Float infinity nextAwayFromZero] raise: Error.
	self assert: Float infinity predecessor nextAwayFromZero = Float infinity.
	self assert: Float infinity predecessor predecessor nextAwayFromZero = Float infinity predecessor.
	self should: [Float negativeInfinity nextAwayFromZero] raise: Error.
	self assert: Float negativeInfinity successor nextAwayFromZero = Float negativeInfinity.
	self assert: Float negativeInfinity successor successor nextAwayFromZero = Float negativeInfinity successor.
	self assert: 0.0 nextAwayFromZero = Float fminDenormalized.
	self assert: -0.0 nextAwayFromZero = Float fminDenormalized negated.! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 4/Dec/2018, 7:16:45 pm (UTC)'!
testNextTowardsZero

	self assert: Float pi nextTowardsZero = Float pi predecessor.
	self assert: Float pi negated nextTowardsZero = Float pi negated successor.
	self assert: 1.0 nextTowardsZero = 1.0 predecessor.
	self should: [Float infinity nextTowardsZero] raise: Error.
	self assert: Float infinity predecessor nextTowardsZero = Float infinity predecessor predecessor.
	self should: [Float negativeInfinity nextTowardsZero] raise: Error.
	self assert: Float negativeInfinity successor nextTowardsZero = Float negativeInfinity successor successor.
	self should: [0.0 nextTowardsZero] raise: Error.
	self assert: 0.0 successor nextTowardsZero = 0.0.
	self should: [-0.0 nextTowardsZero] raise: Error.
	self assert: -0.0 predecessor nextTowardsZero = -0.0! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 5/Apr/2019, 8:26:34 pm (UTC)'!
testPredecessorSuccessor
	self assert: 0.0 successor = Float fminDenormalized.
	self assertIsNegativeZero: 0.0 predecessor.
	self assertIsPositiveZero: -0.0 successor.
	self assert: -0.0 predecessor = Float fminDenormalized negated.
	self assert: 1.0 successor > 1.0.
	self assert: 1.0 successor predecessor = 1.0.
	self assert: 1.0 predecessor < 1.0.
	self assert: 1.0 predecessor successor = 1.0.
	self assert: -1.0 successor > -1.0.
	self assert: -1.0 successor predecessor = -1.0.
	self assert: -1.0 predecessor < -1.0.
	self assert: -1.0 predecessor successor = -1.0.! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 4/Dec/2018, 9:10:45 pm (UTC)'!
testPredecessorSuccessor2
	| mustBeOneOfThem |
	mustBeOneOfThem _ Float fminNormalized + Float fminNormalized successor / 2.
	self assert: (mustBeOneOfThem = Float fminNormalized or: [
		mustBeOneOfThem = Float fminNormalized successor ]).
	mustBeOneOfThem _ 1.0 + 1.0 successor / 2.
	self assert: (mustBeOneOfThem = 1.0 or: [
		mustBeOneOfThem = 1.0 successor ]).
	mustBeOneOfThem _ 1.0 + 1.0 predecessor / 2.
	self assert: (mustBeOneOfThem = 1.0 or: [
		mustBeOneOfThem = 1.0 successor ]).! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 12/Dec/2018, 6:27:52 pm (UTC)'!
testUlp

	{1.0 predecessor. 1.0 successor. Float pi predecessor. Float pi. Float pi successor} do:
		[:f |
		self assert: (f * 2) ulp = (f ulp * 2).
		self assert: (f / 2) ulp = (f ulp / 2).
		self deny: f + f ulp = f.
		self deny: f - f ulp = f.
		"Tests below are valid as long as default rounding mode (to nearest even) is used"
		self assert: f significandAsInteger odd not | (f ulp / 2.0 + f = f successor).
		self assert: f significandAsInteger even not | (f ulp / 2.0 + f = f)].
	
	self assert: 1.0 ulp = 0.9 ulp.
	self deny: 1.0 ulp = 1.1 ulp.
	
	self assert: 0.0 ulp = Float fmin.
	self assert: 1.01  ulp = Float epsilon.
	self assert: Float nan ulp isNaN.
	self assert: Float infinity ulp = Float infinity.
	self assert: Float negativeInfinity ulp = Float infinity.

	self assert: ((0 to: Float precision - 1) allSatisfy: [:each | (Float fmin timesTwoPower: each) ulp = Float fmin]).	! !

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 5/Apr/2019, 8:25:12 pm (UTC)'!
testCopy
	"Elementary tests"
	self assert: 2.0 copy = 2.0.
	self assert: -0.5 copy = -0.5.
	
	"Are exceptional Floats preserved by the copy ?"
	self assert: Float nan copy isNaN.
	self assert: Float infinity copy = Float infinity.
	self assert: Float infinity negated copy = Float infinity negated.
	
	"Is the sign of zero preserved by the copy ?"
	self assertIsPositiveZero: 0.0 copy.
	self assertIsNegativeZero: Float negativeZero copy.! !

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 5/Apr/2019, 11:51:34 pm (UTC)'!
testIsDenormalized
	self deny: Float pi isDenormalized.
	self deny: 0.0 isDenormalized.
	self assert: 0.0 successor isDenormalized.
	self assert: Float fmin isDenormalized.
	self assert: Float fminNormalized predecessor isDenormalized.
	self deny: Float fminNormalized isDenormalized.
	self deny: Float pi negated isDenormalized.
	self deny: -0.0 isDenormalized.
	self assert: -0.0 predecessor isDenormalized.
	self assert: Float fmin negated isDenormalized.
	self assert: Float fminNormalized negated successor isDenormalized.
	self deny: Float fminNormalized negated isDenormalized.! !

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 4/Dec/2018, 6:10:06 pm (UTC)'!
testPartBits
	| denormals exceptionals normals |
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	
	normals , denormals , exceptionals do: [ :aFloat |
		self assert: 
			(Float
				signBit: aFloat signBit
				mantissaBits: aFloat mantissaBits
				exponentBits: aFloat exponentBits) hex
			= aFloat hex ]! !

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'nice 4/May/2014, 2:31:57 am (UTC)'!
testSignificandAndExponent
	| denormals exceptionals normals |
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	
	normals, denormals, exceptionals do: [ :aFloat |
		"Any Float can be decomposed into its significand and exponent, and the significand holds the sign"
		aFloat isNaN
			ifTrue: [self assert: (aFloat significand timesTwoPower: aFloat exponent) isNaN]
			ifFalse: [self
				assert: (aFloat significand timesTwoPower: aFloat exponent)
				equals: aFloat]].
	
	normals , denormals do: [:aFloat |
		"The significand magnitude is in interval [1.0, 2.0) "
		aFloat = 0.0
			ifTrue: [self assert: aFloat significand equals: 0]
			ifFalse: [self
				assert: aFloat significand abs >= 1.0;
				assert: aFloat significand abs < 2.0]]! !

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 3/Oct/2018, 9:41:49 pm (UTC)'!
testSignificandAsInteger
	| mantissaBits denormalPowersOfTwo denormals exceptionals normalPowersOfTwo normals |
	"There are 52 bits used for representing the mantissa (plus an eventual leading 1, see below)"
	mantissaBits := Float precision - 1.
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	normalPowersOfTwo := (-10 to: 10) collect: [:i | 1.0 timesTwoPower: i].
	denormalPowersOfTwo := (Float emin - mantissaBits to: Float emin - 1) collect: [:i | 1.0 timesTwoPower: i].
	
	normals do: [:aNormalFloat |
		"Assume the mantissa is written in least 52 bits of hex format, with an implied 1 on position 53"
		self
			assert: (((Integer readFrom: aNormalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1) bitOr: 1<<mantissaBits)
			equals: aNormalFloat significandAsInteger].
	
	denormals do: [:aDenormalFloat |
		"For zero and denormal, no implied leading one"
		self
			assert: ((Integer readFrom: aDenormalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1)
			equals: aDenormalFloat significandAsInteger].

"This currently is not allowed in Cuis"
false ifTrue: [
	exceptionals do: [:aDenormalOrExceptionalFloat |
		"For every other Float, zero, denormal or exceptional, no implied leading one"
		self
			assert: ((Integer readFrom: aDenormalOrExceptionalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1)
			equals: aDenormalOrExceptionalFloat significandAsInteger].
].

	normalPowersOfTwo do: [:aNormalPowerOfTwoFloat |
		"The significand of a power of two is a power of two, with high bit of expected precision"
		self assert: aNormalPowerOfTwoFloat significandAsInteger isPowerOfTwo.
		self assert: aNormalPowerOfTwoFloat significandAsInteger highBit equals: Float precision.
		self assert: aNormalPowerOfTwoFloat successor significandAsInteger equals: aNormalPowerOfTwoFloat significandAsInteger + 1.
		"The last one is not true for fminNormalized"
		aNormalPowerOfTwoFloat = Float fminNormalized or: [
			self assert: aNormalPowerOfTwoFloat predecessor significandAsInteger equals: aNormalPowerOfTwoFloat significandAsInteger * 2 - 1]].
	
	denormalPowersOfTwo do: [:aDenormalPowerOfTwoFloat |
		"The significand of a denormal power of two is a power of two, just with less bits"
		self assert: aDenormalPowerOfTwoFloat significandAsInteger isPowerOfTwo.
		self assert: aDenormalPowerOfTwoFloat significandAsInteger highBit equals: Float precision + aDenormalPowerOfTwoFloat exponent - Float emin.
		aDenormalPowerOfTwoFloat successor = Float fminNormalized or: [
			self assert: aDenormalPowerOfTwoFloat successor significandAsInteger equals: aDenormalPowerOfTwoFloat significandAsInteger + 1].
		self assert: aDenormalPowerOfTwoFloat predecessor significandAsInteger equals: aDenormalPowerOfTwoFloat significandAsInteger - 1.].
	
	"Well known value for a few extremal cases"
	self assert: Float fmax significandAsInteger equals: 1 << Float precision - 1.
	self assert: Float fmin significandAsInteger equals: 1.
	self assert: 0.0 significandAsInteger equals: 0.
"This currently is not allowed in Cuis"
false ifTrue: [
	self assert: Float infinity significandAsInteger equals: 0.
	self assert: Float nan significandAsInteger > 0
].! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'nice 28/Jan/2017, 2:01:00 am (UTC)'!
testCopySign
	self assert: (0.0 copySignTo: 1) = 1.
	self assert: (Float negativeZero copySignTo: 1) = -1.
	self assertIsNegativeZero: (-1 copySignTo: 0.0).
	self assertIsPositiveZero: (1 copySignTo: Float negativeZero).! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/Apr/2019, 6:27:15 pm (UTC)'!
testIsZero
	self assert: 0.0 isZero.
	self assert: -0.0 isZero.
	self deny:  0.1 isZero.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'nice 27/Jan/2017, 3:45:00 am (UTC)'!
testNegativeZeroAbs
	self assert: Float negativeZero abs signBit = 0 description: 'the absolute value of a negative zero is zero'! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 5/Apr/2019, 12:27:21 pm (UTC)'!
testPositiveAndNegativeZero
	self assert: Float negativeZero = Float zero description: 'Positive and negative zero are defined to be equal'.
	self deny: -0.0 < 0.0 description: 'Positive and negative zero are defined to be equal'.
	self deny: -0.0 > 0.0 description: 'Positive and negative zero are defined to be equal'.
	self deny: Float negativeZero hex = Float zero hex description: 'Positive and negative zero are not the same!!'.
	
	self assertIsPositiveZero: 1.0 * 0.0.
	self assertIsNegativeZero: 1.0 * -0.0.
	self assertIsNegativeZero: -1.0 * 0.0.
	self assertIsPositiveZero: -1.0 * -0.0.

	self assertIsPositiveZero: Float fmin / 4.0.
	self assertIsNegativeZero: Float fmin / -4.0.
	self assertIsNegativeZero: Float fmin negated / 4.0.
	self assertIsPositiveZero: Float fmin negated / -4.0.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'sd 5/Jun/2005, 12:33:00 pm (UTC)'!
testZero1
	"FloatTest new testZero1"

	self assert: Float negativeZero = 0 asFloat.
	self assert: (Float negativeZero at: 1) ~= (0 asFloat at: 1).

	"The negative zero has a bit representation that is different from the bit representation of the positive zero. Nevertheless, both values are defined to be equal."
! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/Apr/2019, 6:32:48 pm (UTC)'!
testZeroAbs
	self assertIsPositiveZero: Float zero abs. 'the absolute value of a positive zero is zero'.
	self assertIsPositiveZero: Float negativeZero abs. 'the absolute value of a negative zero is zero'.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 2/Jul/2019, 3:52:32 pm (UTC)'!
testZeroRaisedToNegativePower
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"

	self should: [0.0 raisedToInteger: -1] raise: ZeroDivide.
	self assert: (0.0 raisedToInteger: -1) isExactly: Float infinity.
	self should: [0.0 raisedTo: -1] raise: ZeroDivide.
	self assert: (0.0 raisedTo: -1) isExactly: Float infinity.
	self should: [0.0 raisedTo: -1.0] raise: ZeroDivide.
	self assert: (0.0 raisedTo: -1.0) isExactly: Float infinity.

	self should: [-0.0 raisedToInteger: -1] raise: ZeroDivide.
	self assert: (-0.0 raisedToInteger: -1) isExactly: Float negativeInfinity.
	self should: [-0.0 raisedTo: -1] raise: ZeroDivide.
	self assert: (-0.0 raisedTo: -1) isExactly: Float negativeInfinity.
	self should: [-0.0 raisedTo: -1.0] raise: ZeroDivide.
	self assert: (-0.0 raisedTo: -1.0) isExactly: Float negativeInfinity.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/Apr/2019, 6:28:40 pm (UTC)'!
testZeroSign
	self assert: Float zero sign = 0.
	self assert: Float negativeZero sign = -1 description: 'negative zero is a zero with non zero sign!!'! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/Apr/2019, 6:33:40 pm (UTC)'!
testZeroSignificandAsInteger
	"This is about http://bugs.squeak.org/view.php?id=6990"

	self assert: 0.0 significandAsInteger = 0.
	self assert: -0.0 significandAsInteger = 0! !

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'sd 5/Jun/2005, 12:30:00 pm (UTC)'!
testInfinity1
   "FloatTest new testInfinity1"

	| i1  i2 |

	i1 := 10000 exp.
	i2 := 1000000000 exp.
	self assert: i1 isInfinite & i2 isInfinite & (i1 = i2).
	"All infinities are equal. (This is a very substantial difference to NaN's, which are never equal."
! !

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'sd 5/Jun/2005, 12:30:00 pm (UTC)'!
testInfinity2
   "FloatTest new testInfinity2"

	| i1  i2 |
	i1 := 10000 exp.
	i2 := 1000000000 exp.
	i2 := 0 - i2. " this is entirely ok. You can compute with infinite values."

	self assert: i1 isInfinite & i2 isInfinite & i1 positive & i2 negative.
	self deny: i1 = i2.
  	"All infinities are signed. Negative infinity is not equal to Infinity"
! !

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'jmv 16/Jun/2026, 1:42:39 pm (UTC)'!
testInfinityTruncated
	"
	self assert: Float infinity truncated isInfinite.
	self assert: Float negativeInfinity truncated isInfinite.
	"
	self should: [ Float infinity truncated ] raise: Error.
	self should: [ Float negativeInfinity truncated ] raise: Error.! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'sd 5/Jun/2005, 12:31:00 pm (UTC)'!
testNaN1
   	"FloatTest new testNaN1"

	self assert: Float nan == Float nan.
	self deny: Float nan = Float nan.
	"a NaN is not equal to itself."
! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 4/Apr/2019, 6:20:45 pm (UTC)'!
testNaN2
	"Two NaN values are always considered to be different.
	On an little-endian machine (32 bit Intel), Float nan is 16rFFF80000 16r00000000.
	On a big-endian machine (PowerPC), Float nan is 16r7FF80000 16r00000000. Changing
	the bit pattern of the first word of a NaN produces another value that is still
	considered equal to NaN. This test should work on both little endian and big
	endian machines."

	"FloatTest new testNaN2"

	| nan1 nan2 |
	nan1 := Float nan copy.
	nan2 := Float nan copy.

	"test two instances of NaN with the same bit pattern"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1.

	"change the bit pattern of nan1"
	self assert: nan1 size = 2.
	self assert: (nan1 at: 2) = 0.
	nan1 at: 1 put: (nan1 at: 1) + 999.
	self assert: nan1 isNaN.
	self assert: nan2 isNaN.
	self deny: (nan1 at: 1) = (nan2 at: 1).

	"test two instances of NaN with different bit patterns"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 4/Apr/2019, 6:21:07 pm (UTC)'!
testNaN3
	"FloatTest new testNaN3"

	| set item identitySet |
	set _ Set new.
	set add: (item _ Float nan).
	self deny: (set includes: item).
	identitySet _ IdentitySet new.
	identitySet add: (item _ Float nan).
	self assert: (identitySet includes: item).! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 23/Apr/2019, 10:20:18 pm (UTC)'!
testNaN4
	"FloatTest new testNaN4"

	| dict |
	dict _ Dictionary new.
	dict
		at: Float nan
		put: #NaN.
	self deny: (dict includesKey: Float nan).! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 13/Mar/2012, 4:34:00 pm (UTC)'!
testNaNCompare
	"IEEE 754 states that NaN cannot be ordered.
	As a consequence, every arithmetic comparison involving a NaN SHOULD return false.
	Except the is different test (~=).
	This test does verify this rule"
	
	| compareSelectors theNaN anotherNaN comparand brokenMethods warningMessage |
	compareSelectors := #(#< #<= #> #>= #=).
	theNaN := Float nan.
	anotherNaN := Float infinity - Float infinity.
	comparand := {1. 2.3. Float infinity. 2/3. 1.25. 2 raisedTo: 50}.
	comparand := comparand , (comparand collect: [:e | e negated]).
	comparand := comparand , {theNaN. anotherNaN}.

"do a first pass to collect all broken methods"
	brokenMethods := Set new.
	comparand do: [:comp |
		compareSelectors do: [:op |
			(theNaN perform: op with: comp) ifTrue: [brokenMethods add: (theNaN class lookupSelector: op)].
			(comp perform: op with: theNaN) ifTrue: [brokenMethods add: (comp class lookupSelector: op)]].
		(theNaN ~= comp) ifFalse: [brokenMethods add: (theNaN class lookupSelector: #~=)].
		(comp ~= theNaN) ifFalse: [brokenMethods add: (comp class lookupSelector: #~=)]].
	
"build a warning message to tell about all broken methods at once"
	warningMessage := String streamContents: [:s |
			s nextPutAll: 'According to IEEE 754 comparing with a NaN should always return false, except ~= that should return true.'; newLine.
			s nextPutAll: 'All these methods failed to do so. They are either broken or call a broken one'.
			brokenMethods do: [:e | s newLine; print: e methodClass; nextPutAll: '>>'; print: e selector]].
		
"Redo the tests so as to eventually open a debugger on one of the failures"
	brokenMethods := Set new.
	comparand do: [:comp2 |
		compareSelectors do: [:op2 |
			self deny: (theNaN perform: op2 with: comp2) description: warningMessage.
			self deny: (comp2 perform: op2 with: theNaN) description: warningMessage].
		self assert: (theNaN ~= comp2) description: warningMessage.
		self assert: (comp2 ~= theNaN) description: warningMessage].! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'sqr 19/Jun/2024, 3:11:33 am (UTC)'!
testNaNPropagationBinary
	"Any operation should propagate NaN"
	| s a c |
	s := Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s := s select: [ :sel | sel numArgs = 1 ].
	s removeAll: #(#at: #basicAt: #ifNotZero: smoothIsAbsBelow:).
	a := s asArray sort.
	a do: [ :sel |
		{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op1 |
			{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op2 |
				op1 isNaN | op2 isNaN ifTrue: [
					c := [ op1 perform: sel with: op2 ] on: UnhandledError do: [ #error ].
					self assert: (c isNumber not or: [c isNaN]) ]]]].! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 19/Nov/2025, 10:05:30 pm (UTC)'!
testNaNPropagationUnary
	"Any operation should propagate NaN"
	| s a c |
	s := Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s := s select: [ :sel | sel numArgs = 0 ].
	s removeAll: #(#asIEEE32BitWord #asIEEE64BitWord #byteSize #hash #identityHash #mantissaBits #signBit #exponentBits #exponent primTestExponent #fractionPart).
	a := s asArray sort.
	a do: [ :sel |
		c := [ Float nan perform: sel ] on: UnhandledError do: [ #error ].
		self assert: (c isNumber not or: [c isNaN]) ].! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 16/Jun/2026, 1:43:01 pm (UTC)'!
testNaNTruncated
	"
	self assert: Float nan truncated isNaN.
	"
	self should: [ Float nan truncated ] raise: Error.! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'nice 15/Mar/2008, 3:42:00 am (UTC)'!
testNaNisLiteral
	self deny: Float nan isLiteral description: 'there is no literal representation of NaN'! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/Apr/2019, 6:02:34 pm (UTC)'!
test32bitConversion
	"Except for NaN, we can convert a 32bits float to a 64bits float exactly.
	Thus we can convert the 64bits float to the original 32bits float pattern."
 
	#(
		16r0 					"zero"
		16r80000000 		"negative zero"
		16r1 					"min denormal"
		16r12345 			"a denormal"
		16r801FEDCB 		"a negative denormal"
		16r7FFFFF 			"largest denormal"
		16r800000 			"smallest normal"
		16r468ACDEF 		"a normal float"
		16rCABD1234 		"a negative normal float"
		16r7F7FFFFF 		"largest finite float"
		16r7F800000 		"positive infinity"
		16rFF800000 		"negative infinity"
		16r803FFFFC
		16r803FFFFD
	)
		do: [ :originalWord | self assert: (Float fromIEEE32Bit: originalWord) asIEEE32BitWord = originalWord ]! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 3/Sep/2020, 9:12:44 pm (UTC)'!
test32bitConversion2
	"This tests the rounding done when going from 64bit Float to 32bit Float
	self new test32bitConversion2
	"

	| pi32 pi32Predecessor roundingTo32Border roundingBorderPrev roundingBorderSuc aux roundedByConversion |
	"Pick two neighbors in the 32-bit Float world"
	pi32 _ Float fromIEEE32Bit: Float pi asIEEE32BitWord.
	pi32Predecessor _ Float fromIEEE32Bit: Float pi asIEEE32BitWord-1.
	"Find the middle value in the 64-bit Float world, and both its neighbors"
	roundingTo32Border _ pi32 + pi32Predecessor / 2.0.
	roundingBorderPrev _ roundingTo32Border predecessor.
	roundingBorderSuc _ roundingTo32Border successor.

	"64 bit middle value is not like any of the 32 bit values. Same for neighbors."
	self deny: roundingTo32Border mantissaPart hex = pi32 mantissaPart hex.
	self deny: roundingTo32Border mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderPrev mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderPrev mantissaPart hex = pi32 mantissaPart hex.
	self deny: roundingBorderSuc mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderSuc mantissaPart hex = pi32 mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingTo32Border asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingTo32Border. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingBorderPrev asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingBorderPrev. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingBorderSuc asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32 mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingBorderSuc. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32 mantissaPart hex! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'nice 30/May/2006, 6:34:00 am (UTC)'!
test32bitGradualUnderflow
	"method asIEEE32BitWord did not respect IEEE gradual underflow"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-126) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"case 1: This example is the first gradual underflow case"
	conv := 2r0.11111111111111111111111e-126 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1 or 0)"
	exponentPart := 0.
	expected := exponentPart bitOr: 2r11111111111111111111111.
	self assert: expected = conv.
	
	"case 2: smallest number"
	conv := 2r0.00000000000000000000001e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
	
	"case 3: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.000000000000000000000011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 4: round to nearest even also in underflow cases... here round to lower"
	conv := 2r0.000000000000000000000101e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 5: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.0000000000000000000001011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r11.
	self assert: expected = conv.
	! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/Apr/2019, 6:15:16 pm (UTC)'!
test32bitInfinities
	| infinityBits negativeInfinityBits |
	infinityBits _ '01111111100000000000000000000000'.
	self assert: (Float infinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = infinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: infinityBits readStream base: 2)) hex = Float infinity hex.
	negativeInfinityBits _ '11111111100000000000000000000000'.
	self assert: (Float negativeInfinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeInfinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: negativeInfinityBits readStream base: 2)) hex = Float negativeInfinity hex.! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/Apr/2019, 6:16:16 pm (UTC)'!
test32bitNaN
	| nanstr |
	
	"check the NaN string representation conforms to IEEE 754"
	nanstr := Float nan asIEEE32BitWord printStringBase: 2 length: 32 padded: true.
	self
		assert: (#($0 $1) includes: (nanstr at: 1));
		assert: (nanstr copyFrom: 2 to: 9) = '11111111';
		assert: (#($0 $1) includes: (nanstr at: 10)); "accept both quiet and signalled NaNs"
		assert: ((nanstr copyFrom: 11 to: 32) reject: [ :c | #($0 $1) includes: c ]) isEmpty.
	
	"check a correct quiet NaN is created from a string"
	self assert: (Float fromIEEE32Bit:
		(Integer readFrom: '01111111110000000000000000000000' readStream base: 2)) isNaN! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'nice 30/May/2006, 4:07:00 am (UTC)'!
test32bitRoundingMode
	"method asIEEE32BitWord did not respect IEEE default rounding mode"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-127) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"This example has two extra bits in mantissa for testing rounding mode
	case 1: should obviously round to upper"
	conv := 2r1.0000000000000000000000111e25 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1)"
	exponentPart := 25+127 bitShift: 23. "127 is 2r01111111 or 16r7F"
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 2: exactly in the mid point of two 32 bit float: round toward nearest even (to upper)"
	conv := 2r1.0000000000000000000000110e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 3: exactly in the mid point of two 32 bit float: round toward nearest even (to lower)"
	conv := 2r1.0000000000000000000000010e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r0.
	self assert: expected = conv.
	
	"case 4: obviously round to upper"
	conv := 2r1.0000000000000000000000011e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 5/Apr/2019, 8:24:45 pm (UTC)'!
test32bitZeros
	| negativeZeroBits zeroBits |
	zeroBits _ '00000000000000000000000000000000'.
	self assert: (Float zero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = zeroBits.
	self assertIsPositiveZero: (Float fromIEEE32Bit: (Integer readFrom: zeroBits readStream base: 2)).
	negativeZeroBits _ '10000000000000000000000000000000'.
	self assert: (Float negativeZero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeZeroBits.
	self assertIsNegativeZero: (Float fromIEEE32Bit: (Integer readFrom: negativeZeroBits readStream base: 2)).! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 24/Apr/2019, 3:48:04 pm (UTC)'!
assert: aFloatingPointNumber isExactly: otherFloatingPointNumber 

    "Both aFloatingPointNumber and otherFloatingPointNumber must be instances
    of our #classTested. Check that and fail if they aren't."

    self assert: (aFloatingPointNumber class inheritsFrom: Float).
    self assert: (otherFloatingPointNumber class inheritsFrom: Float).
    self assert: aFloatingPointNumber = otherFloatingPointNumber.
    self assert: otherFloatingPointNumber = aFloatingPointNumber.
    self assert: aFloatingPointNumber hex = otherFloatingPointNumber hex! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 28/Feb/2019, 8:13:30 pm (UTC)'!
assertIsNaNorFail: aFloatOrSymbol
	"Assert that aFloat is NaN or #fail.
	For some BoxedFloat64, primitive 55 (sqrt) faile, unless jitter, where it answers NaN. We consider both values valid."
	self assert: (aFloatOrSymbol = #fail or: [ aFloatOrSymbol isNaN ])! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 3/Oct/2018, 7:43:27 pm (UTC)'!
assertIsNegativeZero: aFloat
	"Assert that aFloat is Float negativeZero"
	self assert: aFloat hex = Float negativeZero hex! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 3/Oct/2018, 7:43:47 pm (UTC)'!
assertIsPositiveZero: aFloat
	"Assert that aFloat is Float zero (the positive one)"
	self assert: aFloat hex = 0.0 hex! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 27/Feb/2019, 7:25:35 pm (UTC)'!
testPrimAdd

	self assert: (1.0 primTestAdd: 1.0) = 2.0.
	self assert: (Float fminNormalized primTestAdd: Float fminDenormalized) > Float fminNormalized.
	self assert: (Float fminNormalized primTestAdd: Float fminDenormalized) - Float fminNormalized = Float fminDenormalized.
	self assert: (1.0 primTestAdd: Float zero) = 1.0.
	self assert: (Float zero primTestAdd: 1.0) = 1.0.
	self assert: (1.0 primTestAdd: Float negativeZero) = 1.0.
	self assert: (Float negativeZero primTestAdd: 1.0) = 1.0.
	self assert: (1.0 primTestAdd: Float infinity) = Float infinity.
	self assert: (Float infinity primTestAdd: 1.0) = Float infinity.
	self assert: (1.0 primTestAdd: Float negativeInfinity) = Float negativeInfinity.
	self assert: (Float negativeInfinity primTestAdd: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestAdd: Float nan) isNaN.
	self assert: (Float nan primTestAdd: 1.0) isNaN.
	self assert: (Float nan primTestAdd: Float nan) isNaN.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 14/Mar/2019, 6:09:00 pm (UTC)'!
testPrimArcTan

	self verify: 1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / 4.
	self verify: -1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / -4.
	self verify: 0.0 primTestArcTan isTrigonometricallyEqualTo: 0.
	self verify: Float infinity primTestArcTan isTrigonometricallyEqualTo: Float pi / 2.
	self verify: Float negativeInfinity primTestArcTan isTrigonometricallyEqualTo: Float pi / -2.
	self assert: Float nan primTestArcTan isNaN! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 18/Nov/2025, 1:20:48 pm (UTC)'!
testPrimDivideBy

	self assert: (1.5 primTestDivideBy: 2.0) = 0.75.

	Smalltalk numericPrimsMixArithmetic
		ifTrue: [ self assert: (2.0 primTestDivideBy: 1) = 2.0 ]
		ifFalse: [ self assert: (2.0 primTestDivideBy: 1) = #fail ].

	self assert: (2.0 primTestDivideBy: 0) = #fail.
	self assert: (2.0 primTestDivideBy: 0.0) = #fail.
	self assert: (1.2 primTestDivideBy: Float negativeZero) = #fail.
	self assert: (1.2 primTestDivideBy: (1.3 - 1.3)) = #fail.
	self assert: (0.0 primTestDivideBy: 0.0) = #fail.
	self assert: (0.0 primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float negativeZero primTestDivideBy: 0.0) = #fail.
	self assert: (Float negativeZero primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float nan primTestDivideBy: 0.0) = #fail.
	self assert: (Float nan primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float infinity primTestDivideBy: 0.0) = #fail.
	self assert: (Float negativeInfinity primTestDivideBy: Float negativeZero) = #fail.

	self assert: (2.0 primTestDivideBy: 2.0) = 1.0.
	self assert: (Float fminNormalized primTestDivideBy: 2.0) * 2.0 = Float fminNormalized.
	self assert: (Float fminDenormalized * 2.0 primTestDivideBy: 2.0) = Float fminDenormalized.

	self assertIsPositiveZero: (Float zero primTestDivideBy: 1.0).
	self assertIsNegativeZero: (Float negativeZero primTestDivideBy: 1.0).

	self assertIsPositiveZero: (1.0 primTestDivideBy: Float infinity).
	self assert: (Float infinity primTestDivideBy: 1.0) = Float infinity.
	self assertIsNegativeZero: (-1.0 primTestDivideBy: Float infinity).
	self assert: (Float infinity primTestDivideBy: -1.0) = Float negativeInfinity.

	self assertIsNegativeZero: (1.0 primTestDivideBy: Float negativeInfinity).
	self assert: (Float negativeInfinity primTestDivideBy: 1.0) = Float negativeInfinity.
	self assertIsPositiveZero: (-1.0 primTestDivideBy: Float negativeInfinity).
	self assert: (Float negativeInfinity primTestDivideBy: -1.0) = Float infinity.

	self assert: (1.0 primTestDivideBy: Float nan) isNaN.
	self assert: (Float nan primTestDivideBy: 1.0) isNaN.
	self assert: (Float nan primTestDivideBy: Float nan) isNaN.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 10:00:27 pm (UTC)'!
testPrimEqual

	| samples |
	samples _ { Float negativeInfinity. -1.0. Float fminNormalized negated. Float fminDenormalized negated. Float fminDenormalized. Float fminNormalized. 1.0. Float infinity }.

	1 to: samples size do: [ :i |
		1 to: samples size do: [ :j |
			self assert: ((samples at: i) primTestEqual: (samples at: j)) = (i = j ) ]].

	self assert: (Float zero primTestEqual: Float negativeZero).
	self assert: (Float negativeZero primTestEqual: Float zero).
	self deny: (Float zero primTestEqual: Float nan).
	self deny: (Float nan primTestEqual: Float zero).
	self deny: (Float negativeZero primTestEqual: Float nan).
	self deny: (Float nan primTestEqual: Float negativeZero).
	self deny: (Float nan primTestEqual: Float nan).

	1 to: samples size do: [ :i |
		self deny: ((samples at: i) primTestEqual: Float zero).
		self deny: ((samples at: i) primTestEqual: Float negativeZero).
		self deny: ((samples at: i) primTestEqual: Float nan).
		self deny: (Float zero primTestEqual: (samples at: i)).
		self deny: (Float negativeZero primTestEqual: (samples at: i)).
		self deny: (Float nan primTestEqual: (samples at: i)) ].! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 14/Mar/2019, 5:51:33 pm (UTC)'!
testPrimExpAndLn
	| e |
	e := Float e.
	self assert: 1.0 primTestExp = e.
	self assert: e primTestLn = 1.0.
	self verify: 2.0 primTestExp isWithin: 1 floatsAwayFrom: e squared.
	self assert: 2.0 primTestExp sqrt = e.
	self assert: 2.0 primTestExp primTestLn = 2.0.
	self assert: 0.0 primTestExp = 1.0.
	self assert: 0.0 primTestLn = Float negativeInfinity.
	self assert: 0.0 primTestLn primTestExp = 0.0.
	self assert: 0.0 successor primTestLn ~= Float negativeInfinity.
	self assert: 0.0 successor primTestLn primTestExp = 0.0 successor.
	self assert: 0.0 successor primTestLn ceiling asFloat primTestExp > 0.0 successor.
	self assertIsNaNorFail: -1.0 primTestLn.
	self assert: Float nan primTestLn isNaN.
	self assert: Float nan primTestExp isNaN! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 14/Mar/2019, 5:44:44 pm (UTC)'!
testPrimExponent
	self assert: 1.0 primTestExponent = 0.
	self assert: 2.0 predecessor primTestExponent = 0.
	self assert: 2.0 primTestExponent =  1.
	self assert: 4.0 predecessor primTestExponent = 1.
	self assert: 4.0 primTestExponent = 2.
	self assert: Float fminNormalized primTestExponent = -1022.
	self assert: Float fminDenormalized primTestExponent = (-1022-52).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 9:50:52 pm (UTC)'!
testPrimGreater
	self deny: (1.0 primTestGreater: 1.0 successor).
	self deny: (1.0 primTestGreater: 1.0).
	self assert: (1.0 primTestGreater: 1.0 predecessor).
	self assert: (0.0 primTestGreater: Float fminNormalized negated).
	self assert: (0.0 primTestGreater: Float fminDenormalized negated).
	self assert: (0.0 primTestGreater: -1.0).
	self assert: (Float fminNormalized primTestGreater: 0.0).
	self assert: (Float fminDenormalized primTestGreater: 0.0).
	self deny: (-1.0 primTestGreater: 0.0).
	self deny: (1.0 primTestGreater: Float infinity).
	self deny: (Float infinity primTestGreater: Float infinity).
	self deny: (Float negativeInfinity primTestGreater: Float infinity).
	self deny: (Float negativeInfinity primTestGreater: -1.0).
	self deny: (Float negativeInfinity primTestGreater: Float negativeZero).
	self deny: (Float negativeZero primTestGreater: Float zero).
	self deny: (Float zero primTestGreater: Float fminDenormalized).
	self deny: (Float fminDenormalized primTestGreater: Float fminNormalized).
	self deny: (Float fminNormalized primTestGreater: 1.0).
	self deny: (Float nan primTestGreater: Float negativeInfinity).
	self deny: (Float nan primTestGreater: Float negativeZero).
	self deny: (Float nan primTestGreater: Float zero).
	self deny: (Float nan primTestGreater: 1.0).
	self deny: (Float nan primTestGreater: Float infinity).
	self deny: (Float nan primTestGreater: Float nan).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 9:53:17 pm (UTC)'!
testPrimGreaterEqual
	self deny: (1.0 primTestGreaterEqual: 1.0 successor).
	self assert: (1.0 primTestGreaterEqual: 1.0).
	self assert: (1.0 primTestGreaterEqual: 1.0 predecessor).
	self assert: (0.0 primTestGreaterEqual: Float fminNormalized negated).
	self assert: (0.0 primTestGreaterEqual: Float fminDenormalized negated).
	self assert: (0.0 primTestGreaterEqual: -1.0).
	self assert: (Float fminNormalized primTestGreaterEqual: 0.0).
	self assert: (Float fminDenormalized primTestGreaterEqual: 0.0).
	self deny: (-1.0 primTestGreaterEqual: 0.0).
	self deny: (1.0 primTestGreaterEqual: Float infinity).
	self assert: (Float infinity primTestGreaterEqual: Float infinity).
	self deny: (Float negativeInfinity primTestGreaterEqual: Float infinity).
	self deny: (Float negativeInfinity primTestGreaterEqual: -1.0).
	self deny: (Float negativeInfinity primTestGreaterEqual: Float negativeZero).
	self assert: (Float negativeZero primTestGreaterEqual: Float zero).
	self deny: (Float zero primTestGreaterEqual: Float fminDenormalized).
	self deny: (Float fminDenormalized primTestGreaterEqual: Float fminNormalized).
	self deny: (Float fminNormalized primTestGreaterEqual: 1.0).
	self deny: (Float nan primTestGreaterEqual: Float negativeInfinity).
	self deny: (Float nan primTestGreaterEqual: Float negativeZero).
	self deny: (Float nan primTestGreaterEqual: Float zero).
	self deny: (Float nan primTestGreaterEqual: 1.0).
	self deny: (Float nan primTestGreaterEqual: Float infinity).
	self deny: (Float nan primTestGreaterEqual: Float nan).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 9:46:13 pm (UTC)'!
testPrimLess
	self assert: (1.0 primTestLess: 1.0 successor).
	self deny: (1.0 primTestLess: 1.0).
	self deny: (1.0 primTestLess: 1.0 predecessor).
	self assert: (0.0 primTestLess: Float fminNormalized).
	self assert: (0.0 primTestLess: Float fminDenormalized).
	self assert: (0.0 primTestLess: 1.0).
	self assert: (Float fminNormalized negated primTestLess: 0.0).
	self assert: (Float fminDenormalized negated primTestLess: 0.0).
	self assert: (-1.0 primTestLess: 0.0).
	self assert: (1.0 primTestLess: Float infinity).
	self deny: (Float infinity primTestLess: Float infinity).
	self assert: (Float negativeInfinity primTestLess: Float infinity).
	self assert: (Float negativeInfinity primTestLess: -1.0).
	self assert: (Float negativeInfinity primTestLess: Float negativeZero).
	self deny: (Float negativeZero primTestLess: Float zero).
	self assert: (Float zero primTestLess: Float fminDenormalized).
	self assert: (Float fminDenormalized primTestLess: Float fminNormalized).
	self assert: (Float fminNormalized primTestLess: 1.0).
	self deny: (Float nan primTestLess: Float negativeInfinity).
	self deny: (Float nan primTestLess: Float negativeZero).
	self deny: (Float nan primTestLess: Float zero).
	self deny: (Float nan primTestLess: 1.0).
	self deny: (Float nan primTestLess: Float infinity).
	self deny: (Float nan primTestLess: Float nan).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 9:47:25 pm (UTC)'!
testPrimLessEqual
	self assert: (1.0 primTestLessEqual: 1.0 successor).
	self assert: (1.0 primTestLessEqual: 1.0).
	self deny: (1.0 primTestLessEqual: 1.0 predecessor).
	self assert: (0.0 primTestLessEqual: Float fminNormalized).
	self assert: (0.0 primTestLessEqual: Float fminDenormalized).
	self assert: (0.0 primTestLessEqual: 1.0).
	self assert: (Float fminNormalized negated primTestLessEqual: 0.0).
	self assert: (Float fminDenormalized negated primTestLessEqual: 0.0).
	self assert: (-1.0 primTestLessEqual: 0.0).
	self assert: (1.0 primTestLessEqual: Float infinity).
	self assert: (Float infinity primTestLessEqual: Float infinity).
	self assert: (Float negativeInfinity primTestLessEqual: Float infinity).
	self assert: (Float negativeInfinity primTestLessEqual: -1.0).
	self assert: (Float negativeInfinity primTestLessEqual: Float negativeZero).
	self assert: (Float negativeZero primTestLessEqual: Float zero).
	self assert: (Float zero primTestLessEqual: Float fminDenormalized).
	self assert: (Float fminDenormalized primTestLessEqual: Float fminNormalized).
	self assert: (Float fminNormalized primTestLessEqual: 1.0).
	self deny: (Float nan primTestLessEqual: Float negativeInfinity).
	self deny: (Float nan primTestLessEqual: Float negativeZero).
	self deny: (Float nan primTestLessEqual: Float zero).
	self deny: (Float nan primTestLessEqual: 1.0).
	self deny: (Float nan primTestLessEqual: Float infinity).
	self deny: (Float nan primTestLessEqual: Float nan).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 27/Feb/2019, 7:26:33 pm (UTC)'!
testPrimMultiplyBy

	self assert: (2.0 primTestMultiplyBy: 2.0) = 4.0.
	self assert: (Float fminNormalized primTestMultiplyBy: 2.0) = (Float fminNormalized + Float fminNormalized).
	self assert: (Float fminDenormalized primTestMultiplyBy: 2.0) = (Float fminDenormalized + Float fminDenormalized).
	self assertIsPositiveZero: (1.0 primTestMultiplyBy: Float zero).
	self assertIsPositiveZero: (Float zero primTestMultiplyBy: 1.0).
	self assertIsNegativeZero: (1.0 primTestMultiplyBy: Float negativeZero).
	self assertIsNegativeZero: (Float negativeZero primTestMultiplyBy: 1.0).
	self assert: (1.0 primTestMultiplyBy: Float infinity) = Float infinity.
	self assert: (Float infinity primTestMultiplyBy: 1.0) = Float infinity.
	self assert: (1.0 primTestMultiplyBy: Float negativeInfinity) = Float negativeInfinity.
	self assert: (Float negativeInfinity primTestMultiplyBy: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestMultiplyBy: Float nan) isNaN.
	self assert: (Float nan primTestMultiplyBy: 1.0) isNaN.
	self assert: (Float nan primTestMultiplyBy: Float nan) isNaN.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 25/Feb/2019, 10:02:13 pm (UTC)'!
testPrimNotEqual

	| samples |
	samples _ { Float negativeInfinity. -1.0. Float fminNormalized negated. Float fminDenormalized negated. Float fminDenormalized. Float fminNormalized. 1.0. Float infinity }.

	1 to: samples size do: [ :i |
		1 to: samples size do: [ :j |
			self assert: ((samples at: i) primTestNotEqual: (samples at: j)) ~= (i = j ) ]].

	self deny: (Float zero primTestNotEqual: Float negativeZero).
	self deny: (Float negativeZero primTestNotEqual: Float zero).
	self assert: (Float zero primTestNotEqual: Float nan).
	self assert: (Float nan primTestNotEqual: Float zero).
	self assert: (Float negativeZero primTestNotEqual: Float nan).
	self assert: (Float nan primTestNotEqual: Float negativeZero).
	self assert: (Float nan primTestNotEqual: Float nan).

	1 to: samples size do: [ :i |
		self assert: ((samples at: i) primTestNotEqual: Float zero).
		self assert: ((samples at: i) primTestNotEqual: Float negativeZero).
		self assert: ((samples at: i) primTestNotEqual: Float nan).
		self assert: (Float zero primTestNotEqual: (samples at: i)).
		self assert: (Float negativeZero primTestNotEqual: (samples at: i)).
		self assert: (Float nan primTestNotEqual: (samples at: i)) ].! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 14/Mar/2019, 5:57:07 pm (UTC)'!
testPrimSin

	self verify: 0.0 primTestSin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) primTestSin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) primTestSin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) primTestSin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) primTestSin isTrigonometricallyEqualTo: 1.0! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 12/Mar/2019, 6:31:19 pm (UTC)'!
testPrimSmallIntegerAsFloat
	self assert: 7 asFloat = 7.0.
	self assert: 0 asFloat = 0.0.
	self assert: 1 asFloat = 1.0.
	self assert: -1 asFloat = -1.0.
	self assert: Float maxExactInteger asFloat = (Float maxExactInteger printString, '.0') asNumber.
	self assert: Float maxExactInteger negated asFloat = (Float maxExactInteger negated printString, '.0') asNumber.
	self assert: SmallInteger maxVal asFloat = (SmallInteger maxVal printString, '.0') asNumber.
	self assert: SmallInteger minVal asFloat = (SmallInteger minVal printString, '.0') asNumber.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 12/Mar/2019, 6:27:58 pm (UTC)'!
testPrimSqrt

	self assert: 4.0 primTestSqrt = 2.0.
	self assert: 0.0 primTestSqrt = 0.0.
	self assert: Float negativeZero primTestSqrt = 0.0.
	self assert: Float fminNormalized primTestSqrt squared = Float fminNormalized.
	self assert: Float fminDenormalized primTestSqrt squared = Float fminDenormalized.
	self assert: Float infinity primTestSqrt = Float infinity.
	
	"Seems to be #fail in workspaces, but for some BoxedFloat64, Cog seems to answer NaN.
	Never mind. Both are valid. #primSqrt answers NaN in case of primitive failure."
	self assertIsNaNorFail: -4.0 primTestSqrt.
	self assertIsNaNorFail: Float fminNormalized negated primTestSqrt.
	self assertIsNaNorFail: Float fminDenormalized negated primTestSqrt.
	self assertIsNaNorFail: Float negativeInfinity primTestSqrt.
	self assertIsNaNorFail: Float nan primTestSqrt.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 27/Feb/2019, 7:25:52 pm (UTC)'!
testPrimSubtract

	self assert: (3.0 primTestSubtract: 1.0) = 2.0.
	self assert: (Float fminNormalized primTestSubtract: Float fminDenormalized) < Float fminNormalized.
	self assert: (Float fminNormalized primTestSubtract: Float fminDenormalized) - Float fminNormalized = Float fminDenormalized negated.
	self assert: (1.0 primTestSubtract: Float zero) = 1.0.
	self assert: (Float zero primTestSubtract: 1.0) = -1.0.
	self assert: (1.0 primTestSubtract: Float negativeZero) = 1.0.
	self assert: (Float negativeZero primTestSubtract: 1.0) = -1.0.
	self assert: (1.0 primTestSubtract: Float infinity) = Float negativeInfinity.
	self assert: (Float infinity primTestSubtract: 1.0) = Float infinity.
	self assert: (1.0 primTestSubtract: Float negativeInfinity) = Float infinity.
	self assert: (Float negativeInfinity primTestSubtract: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestSubtract: Float nan) isNaN.
	self assert: (Float nan primTestSubtract: 1.0) isNaN.
	self assert: (Float nan primTestSubtract: Float nan) isNaN.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 14/Mar/2019, 5:53:28 pm (UTC)'!
testPrimTimesTwoPower

	self assert: (Float fminNormalized timesTwoPower: -52) = Float fminDenormalized.
	self assert: (Float fminDenormalized timesTwoPower: 52) = Float fminNormalized.
	self assert: (Float fminNormalized timesTwoPower: -53) = 0.0.
	self assert: (0.0 timesTwoPower: 53) = 0.0.

	self assert: (Float fminNormalized timesTwoPower: 1022) = 1.0.
	self assert: (Float fminDenormalized timesTwoPower: 1022+52) = 1.0 .
	self assert: (1.0 timesTwoPower: -1022) = Float fminNormalized.
	self assert:  (2.0 predecessor timesTwoPower: 1023) = Float fmax.

	self assert: (Float pi timesTwoPower: 13) mantissaPart = Float pi mantissaPart.
	
	self assert: (Float infinity timesTwoPower: -1023) = Float infinity.
	self assert: (Float infinity timesTwoPower: -1024) = Float infinity.
	self assert: (Float negativeInfinity timesTwoPower: -1023) = Float negativeInfinity.
	self assert: (Float negativeInfinity timesTwoPower: -1024) = Float negativeInfinity.
	self assert: (Float nan timesTwoPower: 2) isNaN.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 4/Jan/2024, 8:00:48 pm (UTC)'!
testPrimTruncated

	self assert: 1.0 primTestTruncated == 1.
	self assert: 1.1 primTestTruncated == 1.
	self assert: 1.9 primTestTruncated == 1.
	self assert: Float pi primTestTruncated == 3.
	self assert: Float zero primTestTruncated = 0.
	self assert: Float fminNormalized primTestTruncated = 0.
	self assert: Float fminDenormalized primTestTruncated = 0.
	self assert: Float infinity primTestTruncated = #fail.

	self assert: -1.0 primTestTruncated == -1.
	self assert: -1.1 primTestTruncated == -1.
	self assert: -1.9 primTestTruncated == -1.
	self assert: Float pi negated primTestTruncated == -3.
	self assert: Float negativeZero primTestTruncated = 0.
	self assert: Float fminNormalized negated primTestTruncated = 0.
	self assert: Float fminDenormalized negated primTestTruncated = 0.
	self assert: Float negativeInfinity primTestTruncated = #fail.

	self assert: Float nan primTestTruncated = #fail.

	"Only if SmallInteger primitive result is enough to hold the result"
	SmallInteger maxVal >= Float maxExactInteger ifTrue: [
		self assert: (Float maxExactInteger + 0.0) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger + 0.9) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger - 0.1) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger negated + 0.0) primTestTruncated == Float maxExactInteger negated.
		self assert: (Float maxExactInteger negated -  0.9) primTestTruncated == Float maxExactInteger negated.
		self assert: (Float maxExactInteger negated + 0.1) primTestTruncated == Float maxExactInteger negated ].

	self assert: SmallInteger maxVal asFloat predecessor asTrueFraction truncated class = SmallInteger.
	self assert: SmallInteger maxVal asFloat predecessor primTestTruncated = SmallInteger maxVal asFloat predecessor asTrueFraction truncated.	
	self assert: (SmallInteger maxVal+1) asFloat successor asTrueFraction truncated class = LargePositiveInteger.
	self assert: (SmallInteger maxVal+1) asFloat successor primTestTruncated = # fail.
	"Depends on rounding to nearest Float. This could change depending on SmallInteger maxVal.
	Currently false in 64 bits images, but true in 32 bits images."
	SmallInteger maxVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger maxVal asFloat primTestTruncated = SmallInteger maxVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger maxVal asFloat primTestTruncated = #fail ].

	self assert: SmallInteger minVal asFloat successor asTrueFraction truncated class = SmallInteger.
	self assert: SmallInteger minVal asFloat successor primTestTruncated = SmallInteger minVal asFloat successor asTrueFraction truncated.	
	self assert: (SmallInteger minVal-1) asFloat predecessor asTrueFraction truncated class = LargeNegativeInteger.
	self assert: (SmallInteger minVal-1) asFloat predecessor primTestTruncated = # fail.
	"Depends on rounding to nearestFloat. This could change depending on SmallInteger maxVal.
	Currently true in 64 bits images, and in 32 bits images."
	SmallInteger minVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger minVal asFloat primTestTruncated = SmallInteger minVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger minVal asFloat primTestTruncated = #fail ].
! !

!FloatTest methodsFor: 'tests - characterization' stamp: 'nice 5/Mar/2021, 2:34:00 am (UTC)'!
testIsFinite
	self assert: Float fminDenormalized isFinite.
	self assert: 0.0 isFinite.
	self assert: Float pi negated isFinite.
	self assert: Float fmax isFinite.
	
	self deny: Float infinity isFinite.
	self deny: Float negativeInfinity isFinite.
	self deny: Float nan isFinite.! !

!FractionTest methodsFor: 'private' stamp: 'jmv 12/Oct/2011, 2:12:00 am (UTC)'!
assert: a classAndValueEquals: b
	self assert: a class = b class.
	self assert: a = b! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 3/Oct/2018, 7:19:28 pm (UTC)'!
testDegreeCos2
	"self run: #testDegreeCos"
	
	self shouldnt: [ (4/3) degreeCos] raise: Error.
	self assert: (1/3) degreeCos printString =  '0.9999830768577442'! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 3/Oct/2018, 7:19:31 pm (UTC)'!
testDegreeSin2
	"self run: #testDegreeSin"
	
	self shouldnt: [ (4/3) degreeSin] raise: Error.
	self assert: (1/3) degreeSin printString =  '0.005817731354993834'.! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/Oct/2011, 2:27:00 am (UTC)'!
testExactRaisedTo
	"
	FractionTest new testExactRaisedTo
	"
	| f |
	self assert: (4/9 raisedTo: 1/2) classAndValueEquals: 2/3.
	self assert: (9/4 raisedTo: 1/2) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f.
		f _ b / a.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f ].

	self assert: (8/27 raisedTo: 1/3) classAndValueEquals: 2/3.
	self assert: (27/8 raisedTo: 1/3) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated.
		f _ b / a.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated ].

	self assert: (4/9 raisedTo: 3/2) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 2/3) classAndValueEquals: 4/9.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		f _ b / a.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f ].

	self assert: (32/243 raisedTo: 3/5) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 5/3) classAndValueEquals: 32/243.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		self assert: ((f raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(f*f*f*f*f).
		self assert: ((f raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(f*f*f*f*f).

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat.

		f _ b / a.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/Oct/2011, 2:12:00 am (UTC)'!
testExactSqrt
	"
	FractionTest new testExactSqrt
	"
	| f |
	self assert: (4/9) sqrt classAndValueEquals: 2/3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :i :j |
		f _ i / j.
		self assert: f squared sqrt classAndValueEquals: f.
		f _ j / i.
		self assert: f squared sqrt classAndValueEquals: f ]! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 11/Dec/2012, 11:56:00 pm (UTC)'!
testFloorLog
	self assert: (1/100 floorLog: 10) = -2.
	self assert: (((2 raisedTo: Float emax + 11)/3) floorLog: 10)
		= ((Float emax + 11)*2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) floorLog: 10)
		= ((Float emin - Float precision)*2 log + 3 log) floor description: 'Fraction>>log should not underflow'! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/Dec/2012, 2:27:00 am (UTC)'!
testFloorLogExactness

	1 + (Float fminDenormalized floorLog: 10) to: -1 do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	1/(10 raisedTo: n) asFloat is not exact
	(aFloat log: radix) is not exact

	(1 + (Float fminDenormalized floorLog: 10) to: -1) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."
	! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 14/Oct/2011, 1:38:00 am (UTC)'!
testInexactRaisedTo
	"
	FractionTest new testInexactRaisedTo
	"
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) raisedTo: 1/3) = 1.0.
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/3) = -1.0! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 14/Oct/2011, 1:27:00 am (UTC)'!
testInexactSqrt
	"
	FractionTest new testInexactSqrt
	"
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) sqrt = 1.0! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 18/Dec/2018, 2:42:49 pm (UTC)'!
testLn
	self verify: (1/100) ln isWithinOneFloatAwayFrom: -2 * 10 ln.
	self verify: ((2 raisedTo: Float emax + 11) / 3) ln isWithinOneFloatAwayFrom: (Float emax + 11) * 2 ln - 3 ln.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) ln isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 ln + 3 ln! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 18/Dec/2018, 2:45:24 pm (UTC)'!
testLog
	self assert: (1/100) log = -2.
	self verify: ((2 raisedTo: Float emax + 11) / 3) log isWithinOneFloatAwayFrom: (Float emax + 11) * 2 log - 3 log.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) log isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 log + 3 log.! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 20/Oct/2011, 12:48:00 am (UTC)'!
testNthRoot
	self assert: ((-2 raisedTo: 35) / (3 raisedTo: 20) raisedTo: 1/5) equals: (-2 raisedTo: 7) / (3 raisedTo: 4).
	self assert: (1 / (1 << 2000) raisedTo: 1/100) equals: 1 / (1 << 20)! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 24/May/2024, 7:16:15 pm (UTC)'!
testSinCosForHugeValues1
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e22 + (10202/10000).
	equivalentFloatInTwoPiRange := 0.0.
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e22 + (10202/10000) + i.
		equivalentFloatInTwoPiRange := 0.0 + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].
! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 24/May/2024, 7:17:54 pm (UTC)'!
testSinCosForHugeValues2
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e500 - (282532 / 100000).
	equivalentFloatInTwoPiRange := 0.0.
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e500 - (282532 / 100000) + i.
		equivalentFloatInTwoPiRange := 0.0 + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].
! !

!FractionTest methodsFor: 'tests - printing' stamp: 'nice 24/Jul/2010, 10:43:00 pm (UTC)'!
testFractionPrinting

	self assert: (353/359) printString = '(353/359)'.
	self assert: ((2/3) printStringBase: 2) = '(10/11)'.
	self assert: ((2/3) storeStringBase: 2) = '(2r10/2r11)'.
	self assert: ((5/7) printStringBase: 3) = '(12/21)'.
	self assert: ((5/7) storeStringBase: 3) = '(3r12/3r21)'.
	self assert: ((11/13) printStringBase: 4) = '(23/31)'.
	self assert: ((11/13) storeStringBase: 4) = '(4r23/4r31)'.
	self assert: ((17/19) printStringBase: 5) = '(32/34)'.
	self assert: ((17/19) storeStringBase: 5) = '(5r32/5r34)'.
	self assert: ((23/29) printStringBase: 6) = '(35/45)'.
	self assert: ((23/29) storeStringBase: 6) = '(6r35/6r45)'.
	self assert: ((31/37) printStringBase: 7) = '(43/52)'.
	self assert: ((31/37) storeStringBase: 7) = '(7r43/7r52)'.
	self assert: ((41/43) printStringBase: 8) = '(51/53)'.
	self assert: ((41/43) storeStringBase: 8) = '(8r51/8r53)'.
	self assert: ((47/53) printStringBase: 9) = '(52/58)'.
	self assert: ((47/53) storeStringBase: 9) = '(9r52/9r58)'.
	self assert: ((59/61) printStringBase: 10) = '(59/61)'.
	self assert: ((59/61) storeStringBase: 10) = '(59/61)'.
	self assert: ((67/71) printStringBase: 11) = '(61/65)'.
	self assert: ((67/71) storeStringBase: 11) = '(11r61/11r65)'.
	self assert: ((73/79) printStringBase: 12) = '(61/67)'.
	self assert: ((73/79) storeStringBase: 12) = '(12r61/12r67)'.
	self assert: ((83/89) printStringBase: 13) = '(65/6B)'.
	self assert: ((83/89) storeStringBase: 13) = '(13r65/13r6B)'.
	self assert: ((97/101) printStringBase: 14) = '(6D/73)'.
	self assert: ((97/101) storeStringBase: 14) = '(14r6D/14r73)'.
	self assert: ((103/107) printStringBase: 15) = '(6D/72)'.
	self assert: ((103/107) storeStringBase: 15) = '(15r6D/15r72)'.
	self assert: ((109/113) printStringBase: 16) = '(6D/71)'.
	self assert: ((109/113) storeStringBase: 16) = '(16r6D/16r71)'.
	self assert: ((127/131) printStringBase: 17) = '(78/7C)'.
	self assert: ((127/131) storeStringBase: 17) = '(17r78/17r7C)'.
	self assert: ((137/139) printStringBase: 18) = '(7B/7D)'.
	self assert: ((137/139) storeStringBase: 18) = '(18r7B/18r7D)'.
	self assert: ((149/151) printStringBase: 19) = '(7G/7I)'.
	self assert: ((149/151) storeStringBase: 19) = '(19r7G/19r7I)'.
	self assert: ((157/163) printStringBase: 20) = '(7H/83)'.
	self assert: ((157/163) storeStringBase: 20) = '(20r7H/20r83)'.
	self assert: ((167/173) printStringBase: 21) = '(7K/85)'.
	self assert: ((167/173) storeStringBase: 21) = '(21r7K/21r85)'.
	self assert: ((179/181) printStringBase: 22) = '(83/85)'.
	self assert: ((179/181) storeStringBase: 22) = '(22r83/22r85)'.
	self assert: ((191/193) printStringBase: 23) = '(87/89)'.
	self assert: ((191/193) storeStringBase: 23) = '(23r87/23r89)'.
	self assert: ((197/199) printStringBase: 24) = '(85/87)'.
	self assert: ((197/199) storeStringBase: 24) = '(24r85/24r87)'.
	self assert: ((211/223) printStringBase: 25) = '(8B/8N)'.
	self assert: ((211/223) storeStringBase: 25) = '(25r8B/25r8N)'.
	self assert: ((227/229) printStringBase: 26) = '(8J/8L)'.
	self assert: ((227/229) storeStringBase: 26) = '(26r8J/26r8L)'.
	self assert: ((233/239) printStringBase: 27) = '(8H/8N)'.
	self assert: ((233/239) storeStringBase: 27) = '(27r8H/27r8N)'.
	self assert: ((241/251) printStringBase: 28) = '(8H/8R)'.
	self assert: ((241/251) storeStringBase: 28) = '(28r8H/28r8R)'.
	self assert: ((257/263) printStringBase: 29) = '(8P/92)'.
	self assert: ((257/263) storeStringBase: 29) = '(29r8P/29r92)'.
	self assert: ((269/271) printStringBase: 30) = '(8T/91)'.
	self assert: ((269/271) storeStringBase: 30) = '(30r8T/30r91)'.
	self assert: ((277/281) printStringBase: 31) = '(8T/92)'.
	self assert: ((277/281) storeStringBase: 31) = '(31r8T/31r92)'.
	self assert: ((283/293) printStringBase: 32) = '(8R/95)'.
	self assert: ((283/293) storeStringBase: 32) = '(32r8R/32r95)'.
	self assert: ((307/311) printStringBase: 33) = '(9A/9E)'.
	self assert: ((307/311) storeStringBase: 33) = '(33r9A/33r9E)'.
	self assert: ((313/317) printStringBase: 34) = '(97/9B)'.
	self assert: ((313/317) storeStringBase: 34) = '(34r97/34r9B)'.
	self assert: ((331/337) printStringBase: 35) = '(9G/9M)'.
	self assert: ((331/337) storeStringBase: 35) = '(35r9G/35r9M)'.
	self assert: ((347/349) printStringBase: 36) = '(9N/9P)'.
	self assert: ((347/349) storeStringBase: 36) = '(36r9N/36r9P)'.

	self assert: ((-2/3) printStringBase: 2) = '(-10/11)'.
	self assert: ((-2/3) storeStringBase: 2) = '(-2r10/2r11)'.
	self assert: ((5 / -7) printStringBase: 3) = '(-12/21)'.
	self assert: ((5 / -7) storeStringBase: 3) = '(-3r12/3r21)'.
! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:56:00 pm (UTC)'!
testIntegerWholeDivision
	
	self assert: 4 / (2/3) classAndValueEquals: 6.
	
	self assert: 4 / (-2/3) classAndValueEquals: -6.
	
	self assert: -4 / (-2/3) classAndValueEquals: 6.
	
	self assert: -4 / (2/3) classAndValueEquals: -6.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:56:00 pm (UTC)'!
testIntegerWholeMultiplication
	
	self assert: 4 * (3/2) classAndValueEquals: 6.
	
	self assert: 4 * (-3/2) classAndValueEquals: -6.
	
	self assert: -4 * (-3/2) classAndValueEquals: 6.
	
	self assert: -4 * (3/2) classAndValueEquals: -6.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:56:00 pm (UTC)'!
testReciprocal

	self 
		assert: (1/2) reciprocal classAndValueEquals: 2;
		assert: (3/4) reciprocal equals: (4/3);
		assert: (-1/3) reciprocal classAndValueEquals: -3;
		assert: (-3/5) reciprocal equals: (-5/3)! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:56:00 pm (UTC)'!
testWholeDifference
	
	self assert: (2/3) - (5/3) classAndValueEquals: -1.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:56:00 pm (UTC)'!
testWholeDivision
	
	self assert: (3/2) / (3/4) classAndValueEquals: 2.
	
	self assert: (3/2) / (-3/4) classAndValueEquals: -2.
	
	self assert: (-3/2) / (-3/4) classAndValueEquals: 2.
	
	self assert: (-3/2) / (3/4) classAndValueEquals: -2.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:57:00 pm (UTC)'!
testWholeMultiplication
	
	self assert: (3/2) * (4/3) classAndValueEquals: 2.
	
	self assert: (3/2) * (-4/3) classAndValueEquals: -2.
	
	self assert: (-3/2) * (-4/3) classAndValueEquals: 2.
	
	self assert: (-3/2) * (4/3) classAndValueEquals: -2.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 22/Jan/2012, 10:57:00 pm (UTC)'!
testWholeSum
	
	self assert: (5/3) + (1/3) classAndValueEquals: 2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 4/Jun/2011, 1:32:00 am (UTC)'!
testCeiling
	self assert: (3 / 2) ceiling = 2.
	self assert: (-3 / 2) ceiling = -1.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 4/Jun/2011, 1:32:00 am (UTC)'!
testFloor
	self assert: (3 / 2) floor = 1.
	self assert: (-3 / 2) floor = -2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 4/Jun/2011, 1:39:00 am (UTC)'!
testRounded
	self assert: (4 / 5) rounded = 1.
	self assert: (6 / 5) rounded = 1.
	self assert: (-4 / 5) rounded = -1.
	self assert: (-6 / 5) rounded = -1.
	
	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded = 2.
	self assert: (-3 / 2) rounded = -2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 4/Jun/2011, 1:35:00 am (UTC)'!
testTruncated
	self assert: (3 / 2) truncated = 1.
	self assert: (-3 / 2) truncated = -1.! !

!FractionTest methodsFor: 'tests - invariants' stamp: 'nice 22/Jan/2012, 11:07:00 pm (UTC)'!
testThatFractionDenominatorIsPositive
	self assert: (-3 / 2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (-3 / 2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (3 / -2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (-3 / -2) numerator positive description: 'two negative signs are simplified'.
	self assert: (-3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.! !

!FractionTest methodsFor: 'tests - invariants' stamp: 'nice 22/Jan/2012, 11:12:00 pm (UTC)'!
testThatFractionIsReduced
	self assert: (4 / 6) numerator equals: 2.
	self assert: (4 / 6) denominator equals: 3.
	
	self assert: (4 / 2) classAndValueEquals: 2.
	
	"Fraction class>>#numerator:denominator: does not automatically reduce the Fraction.
	Since it does not guaranty above invariant, it must be used with care."
	self assert: (Fraction numerator: 4 denominator: 6) numerator equals: 4.
	self assert: (Fraction numerator: 4 denominator: 6) denominator equals: 6.
	self assert: (Fraction numerator: 4 denominator: 6) reduced numerator equals: 2.
	self assert: (Fraction numerator: 4 denominator: 6) reduced denominator equals: 3.! !

!FractionTest methodsFor: 'testing' stamp: 'jmv 9/Oct/2018, 1:37:12 pm (UTC)'!
testHash

	| a fraction1 fraction2 |
	fraction1 _ 1/3.
	fraction2 _ (1/3) + (1e-1000).
	self deny: fraction1 = fraction2 description: 'precondition'.
	self assert: fraction1 asFloat = fraction2 asFloat description: 'precondition'.
	a _ Set new.
	a add: fraction1; add: fraction2.
	self assert: a size = 2! !

!FractionTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:20:34 am (UTC)'!
testIsType
	self assert: ((1 / 2100) is: #Fraction).
	self assert: ((1 / 2100) is: #Number).! !

!FractionTest methodsFor: 'tests - rounding' stamp: 'jmv 29/Oct/2021, 3:13:20 pm (UTC)'!
testRoundHalfAwayFromZero
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero"
	self assert: (1/2) roundedHAFZ = 1.
	self assert: (3/2) roundedHAFZ = 2.
	self assert: (-1/2) roundedHAFZ = -1.
	self assert: (-3/2) roundedHAFZ = -2.
	self assert: (((0 to: 5 by: 1/4) collect: [ :f | f roundedHAFZ ]) = #(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5) ).
	self assert: (((0 to: -5 by: -1/4) collect: [ :f | f roundedHAFZ ]) = #(0 0 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3 -3 -3 -4 -4 -4 -4 -5 -5 -5) ).! !

!FractionTest methodsFor: 'tests - rounding' stamp: 'jmv 9/Oct/2018, 8:12:18 pm (UTC)'!
testRoundHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: (1/2) rounded = 0.
	self assert: (3/2) rounded = 2.
	self assert: (-1/2) rounded = 0.
	self assert: (-3/2) rounded = -2.
	self assert: (((0 to: 5 by: 1/4) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((0 to: -5 by: -1/4) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) ).! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:12:00 am (UTC)'!
testAndSingleBitWithMinusOne
	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i)].! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'nice 10/Jan/2010, 9:46:00 pm (UTC)'!
testLargeShift
	"A sanity check for LargeInteger bitShifts"
	
	| suite |
	suite := #(	"some numbers on 64 bits or less"
		'101101011101001100110111110110011101101101000001110110011'
		'1101101001100010011001101110100000111011011010100011101100'
		'101101101011110011001100110011011101011001111000100011101000'
		'10101101101000101001111111111100101101011001011000100011100000'
		'1000101010101001111011101010111001011111110011110001000110000000'
		'1100101010101000010011101000110010111110110011110000000000000001' ).
	"65 bits or less"
	suite := suite , (suite collect: [:e | '1' , e reversed ]).
	"129 bits or less"
	suite := suite , (suite collect: [:e | e ,e ]).
	suite do: [:bits | | num ls rs |
		num := Integer readFrom: bits readStream base: 2.
		0 to: bits size-1 do: [:shift |
			ls := (num bitShift: shift) printStringBase: 2.
			rs := (num bitShift: 0-shift) printStringBase: 2.
			self assert: ls = (bits , (String new: shift withAll: $0)).
			self assert: rs = (bits copyFrom: 1 to: bits size - shift).
			]].! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:13:00 am (UTC)'!
testMixedSignDigitLogic
	"Verify that mixed sign logic with large integers works."
	self assert: (-2 bitAnd: 16rFFFFFFFF) = 16rFFFFFFFE! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:12:00 am (UTC)'!
testNBitAndNNegatedEqualsN
	"Verify that (n bitAnd: n negated) = n for single bits"
	| n |
	1 to: 100 do: [:i | n := 1 bitShift: i.
				self assert: (n bitAnd: n negated) = n]! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:12:00 am (UTC)'!
testNNegatedEqualsNComplementedPlusOne
	"Verify that n negated = (n complemented + 1) for single bits"
	| n |
	1 to: 100 do: [:i | n := 1 bitShift: i.
				self assert: n negated = ((n bitXor: -1) + 1)]! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:13:00 am (UTC)'!
testShiftMinusOne1LeftThenRight
	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((-1 bitShift: i) bitShift: i negated) = -1].
! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 8/Jan/2002, 1:12:00 am (UTC)'!
testShiftOneLeftThenRight
	"Shift 1 bit left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) = 1].
! !

!IntegerTest methodsFor: 'private' stamp: 'jmv 11/Oct/2011, 12:14:00 pm (UTC)'!
assert: a classAndValueEquals: b
	self assert: a class = b class.
	self assert: a = b! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:01:00 pm (UTC)'!
testBackslashBackslashLnLn

	self assert: -42391158275216203514294433200 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: -42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ -14130386091738734504764811067 = -1.
	self assert: -8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -79766443076872509863360.
	self assert: -8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:00:00 pm (UTC)'!
testBackslashBackslashLnLp

	self assert: -42391158275216203514294433200 \\ 14130386091738734504764811067 = 1.
	self assert: -42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: -8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 1.
	self assert: -8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:01:00 pm (UTC)'!
testBackslashBackslashLnSn

	self assert: -42391158275216203514294433201 \\ -3 = 0.
	self assert: -42391158275216203514294433202 \\ -3 = -1.
	self assert: -42391158275216203514294433203 \\ -3 = -2.
	self assert: -42391158275216203514294433204 \\ -3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:02:00 pm (UTC)'!
testBackslashBackslashLnSp

	self assert: -42391158275216203514294433201 \\ 3 = 0.
	self assert: -42391158275216203514294433202 \\ 3 = 2.
	self assert: -42391158275216203514294433203 \\ 3 = 1.
	self assert: -42391158275216203514294433204 \\ 3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:04:00 pm (UTC)'!
testBackslashBackslashLpLn

	self assert: 42391158275216203514294433200 \\ -14130386091738734504764811067 = -1.
	self assert: 42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: 8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -1.
	self assert: 8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:05:00 pm (UTC)'!
testBackslashBackslashLpLp

	self assert: 42391158275216203514294433200 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: 42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ 14130386091738734504764811067 = 1.
	self assert: 8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 79766443076872509863360.
	self assert: 8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:06:00 pm (UTC)'!
testBackslashBackslashLpSn

	self assert: 42391158275216203514294433201 \\ -3 = 0.
	self assert: 42391158275216203514294433202 \\ -3 = -2.
	self assert: 42391158275216203514294433203 \\ -3 = -1.
	self assert: 42391158275216203514294433204 \\ -3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:07:00 pm (UTC)'!
testBackslashBackslashLpSp

	self assert: 42391158275216203514294433201 \\ 3 = 0.
	self assert: 42391158275216203514294433202 \\ 3 = 1.
	self assert: 42391158275216203514294433203 \\ 3 = 2.
	self assert: 42391158275216203514294433204 \\ 3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:08:00 pm (UTC)'!
testBackslashBackslashSnLn

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: -1 \\ -42391158275216203514294433201 = -1.
	self assert: -14348907 \\ -42391158275216203514294433201 = -14348907! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:09:00 pm (UTC)'!
testBackslashBackslashSnLp

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: -1 \\ 42391158275216203514294433201 =  42391158275216203514294433200.
	self assert: -14348907 \\ 42391158275216203514294433201 = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:09:00 pm (UTC)'!
testBackslashBackslashSnSn

	self assert: 0 \\ -3 = 0.
	self assert: -1 \\ -3 = -1.
	self assert: -9 \\ -3 = 0.
	self assert: -10 \\ -3 = -1.
	self assert: -11 \\ -3 = -2.
	self assert: -12 \\ -3 = 0.
	self assert: -13 \\ -3 = -1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:10:00 pm (UTC)'!
testBackslashBackslashSnSp

	self assert: 0 \\ 3 = 0.
	self assert: -1 \\ 3 = 2.
	self assert: -9 \\ 3 = 0.
	self assert: -10 \\ 3 = 2.
	self assert: -11 \\ 3 = 1.
	self assert: -12 \\ 3 = 0.
	self assert: -13 \\ 3 = 2! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:11:00 pm (UTC)'!
testBackslashBackslashSpLn

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: 1 \\ -42391158275216203514294433201 = -42391158275216203514294433200.
	self assert: 14348907 \\ -42391158275216203514294433201 = -42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:12:00 pm (UTC)'!
testBackslashBackslashSpLp

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: 1 \\ 42391158275216203514294433201 = 1.
	self assert: 14348907 \\ 42391158275216203514294433201 = 14348907! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:13:00 pm (UTC)'!
testBackslashBackslashSpSn

	self assert: 0 \\ -3 = 0.
	self assert: 1 \\ -3 = -2.
	self assert: 9 \\ -3 = 0.
	self assert: 10 \\ -3 = -2.
	self assert: 11 \\ -3 = -1.
	self assert: 12 \\ -3 = 0.
	self assert: 13 \\ -3 = -2! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 25/Jan/2014, 6:13:00 pm (UTC)'!
testBackslashBackslashSpSp

	self assert: 0 \\ 3 = 0.
	self assert: 1 \\ 3 = 1.
	self assert: 9 \\ 3 = 0.
	self assert: 10 \\ 3 = 1.
	self assert: 11 \\ 3 = 2.
	self assert: 12 \\ 3 = 0.
	self assert: 13 \\ 3 = 1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'jmv 24/May/2024, 6:55:26 pm (UTC)'!
testModuloTwoPiAsFloat

	| k |
	k := 1e22.
	self assert: k moduloTwoPiAsFloat + 1 = 	(k+1) moduloTwoPiAsFloat.
	k negated moduloTwoPiAsFloat + 1 = 	(k negated+1) moduloTwoPiAsFloat.
	k moduloTwoPiAsFloat + k negated moduloTwoPiAsFloat = Float twoPi.
	(k+1) moduloTwoPiAsFloat + (k negated-1) moduloTwoPiAsFloat = Float twoPi.
	(k-1) moduloTwoPiAsFloat + (k negated+1) moduloTwoPiAsFloat = Float twoPi.

	k := 1e500.
	self assert: k moduloTwoPiAsFloat + 1 = 	(k+1) moduloTwoPiAsFloat.
	k negated moduloTwoPiAsFloat + 1 = 	(k negated+1) moduloTwoPiAsFloat.
	k moduloTwoPiAsFloat + k negated moduloTwoPiAsFloat = Float twoPi.
	(k+1) moduloTwoPiAsFloat + (k negated-1) moduloTwoPiAsFloat = Float twoPi.
	(k-1) moduloTwoPiAsFloat + (k negated+1) moduloTwoPiAsFloat = Float twoPi.
! !

!IntegerTest methodsFor: 'tests - benchmarks' stamp: 'sd 5/Jun/2005, 12:37:00 pm (UTC)'!
testBenchFib

	self assert: (0 benchFib = 1).
	self assert: (1 benchFib = 1).
	self assert: (2 benchFib = 3).
	! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 22/Aug/2016, 12:02:52 am (UTC)'!
testBigReceiverInexactNthRoot
	"
	IntegerTest new testBigReceiverInexactNthRoot
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum _ (100 factorial raisedTo: 3) + 1.		"Add 1 so it is not a whole cube"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result _ bigNum nthRoot: 3.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following line"
	self assert: 100 factorial asFloat = (100 factorial+1) asFloat! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 22/Aug/2016, 12:03:04 am (UTC)'!
testBigReceiverInexactSqrt
	"
	IntegerTest new testBigReceiverInexactSqrt
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum _ 100 factorial squared + 1.		"Add 1 so it is not a whole square"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result _ bigNum sqrt.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following lines"
	self assert: (result successor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.
	self assert: (result predecessor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 14/Oct/2011, 1:46:00 am (UTC)'!
testExactRaisedTo
	"
	IntegerTest new testExactRaisedTo
	"
	self assert: (4 raisedTo: 1/2) classAndValueEquals: 2.
	self assert: (9 raisedTo: 1/2) classAndValueEquals: 3.
	self assert: (9 raisedTo: -1/2) classAndValueEquals: 1/3.
	self assert: (-1 raisedTo: 1/3) classAndValueEquals: -1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: (i squared raisedTo: 1/2) classAndValueEquals: i.
		self assert: (i negated squared raisedTo: 1/2) classAndValueEquals: i ].

	self assert: (8 raisedTo: 1/3) classAndValueEquals: 2.
	self assert: (27 raisedTo: 1/3) classAndValueEquals: 3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i.
		self assert: ((i negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i negated ].

	self assert: (4 raisedTo: 3/2) classAndValueEquals: 8.
	self assert: (8 raisedTo: 2/3) classAndValueEquals: 4.
	self assert: (8 raisedTo: -2/3) classAndValueEquals: 1/4.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i.
		self assert: ((i negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i ].

	self assert: (32 raisedTo: 3/5) classAndValueEquals: 8.
	self assert: (8 raisedTo: 5/3) classAndValueEquals: 32.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 5) raisedTo: 3/5) classAndValueEquals: i*i*i.
		self assert: ((i raisedTo: 3) raisedTo: 5/3) classAndValueEquals: i*i*i*i*i.
		self assert: ((i negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (i*i*i) negated.
		self assert: ((i negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (i*i*i*i*i) negated.

		self assert: ((i raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(i*i*i*i*i).

		self assert: ((i raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(i*i*i*i*i).

		"No exact result => Float result"
		self assert: ((i raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((i negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/Oct/2011, 2:09:00 am (UTC)'!
testExactSqrt
	"
	IntegerTest new testExactSqrt
	"
	self assert: 4 sqrt classAndValueEquals: 2.
	self assert: 9 sqrt classAndValueEquals: 3.
	self assert: Float maxExactInteger squared sqrt classAndValueEquals: Float maxExactInteger.
	self assert: (Float maxExactInteger+1) squared sqrt classAndValueEquals: Float maxExactInteger+1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: i squared sqrt classAndValueEquals: i ]! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 11/Dec/2012, 11:53:00 pm (UTC)'!
testFloorLog
	self assert: (100 floorLog: 10) = 2.
	self assert: (((2 raisedTo: Float emax + 3) floorLog: 10) = (2 log*(Float emax + 3)) floor) description: 'Integer>>floorLog: should not overflow'! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/Dec/2012, 2:30:00 am (UTC)'!
testFloorLogExactness

	1 to: (Float fmax floorLog: 10) do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	(10 raisedTo: n) asFloat is not exact for n > 22
	(aFloat log: radix) is not exact

	(1 to: (Float fmax floorLog: 10)) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 18/Dec/2018, 2:47:37 pm (UTC)'!
testLn
	self verify: 100 ln isWithinOneFloatAwayFrom: 10 ln*2.
	self verify: (2 raisedTo: Float emax + 3) ln isWithinOneFloatAwayFrom: 2 ln * (Float emax + 3)! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 18/Dec/2018, 2:48:49 pm (UTC)'!
testLog
	self assert: 100 log = 2.
	self verify: (2 raisedTo: Float emax + 3) log isWithinOneFloatAwayFrom: 2 log * (Float emax + 3)! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 20/Oct/2011, 2:48:00 am (UTC)'!
testNthRoot
	"
	IntegerTest new testNthRoot
	"
	| i |
	i _ 1234987687234509123.
	#(3 5 7 9 11 13 15 17 19 21 23 25 27) do: [ :n |
		self assert: ((i raisedTo: n) nthRoot: n) = i ].

	self shouldnt: [ (1 << 2000 nthRoot: 100) ] raise: ArithmeticError.
	self assert: (1 << 2000 nthRoot: 100) equals: 1 << 20! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 13/Oct/2011, 1:09:00 pm (UTC)'!
testNthRootErrorConditions
	"
	IntegerTest new testExactRaisedToErrorConditions
	"

	self should: [ -2 nthRoot: 1/4 ] raise: ArithmeticError.
	self should: [ -2 nthRoot: 1.24 ] raise: ArithmeticError.! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 15/Mar/2014, 6:50:00 pm (UTC)'!
testNthRootExactness
	| inexactRoots largeRaisedTo6 |
	largeRaisedTo6 := (2 to: 100) collect: [:k | (k raisedTo: 11) raisedTo: 6].
	inexactRoots := largeRaisedTo6 reject: [:e | (e nthRoot: 6) isInteger].
	self assert: inexactRoots isEmpty description: 'Failed to find the exact 6th root of these numbers'! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 4/May/2019, 4:30:00 am (UTC)'!
testNthRootRounded
	<timeout:  5 "seconds">
	| d x |
	d := ((1 + 2 reciprocal - (1<<30) reciprocal) raisedTo: 1000) floor. "close to 1.5 by default"
	x := ((1 + 2 reciprocal + (1<<30) reciprocal) raisedTo: 1000) ceiling. "close to 1.5 by excess"
	self assert: (d nthRootTruncated: 1000) equals: 1.
	self assert: (x nthRootTruncated: 1000) equals: 1.
	self assert: (d nthRootRounded: 1000) equals: 1.
	self assert: (x nthRootRounded: 1000) equals: 2.! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 20/Oct/2011, 1:03:00 am (UTC)'!
testNthRootTruncated
	<timeout:  5 "seconds">
	| tooBigToBeAFloat large |
	tooBigToBeAFloat := 1 << 2000.
	self assert: (tooBigToBeAFloat nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat + 1 nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat - 1 nthRootTruncated: 100) equals: 1 << 20 - 1.
	
	large := -3 raisedTo: 255.
	self assert: (large nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	self assert: (large + 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15) + 1.
	self assert: (large - 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	
	2 to: 10 do: [:thePower |
		1 to: 10000 do: [:n |
			| theTruncatedRoot |
			theTruncatedRoot := n nthRootTruncated: thePower.
			self assert: (theTruncatedRoot raisedTo: thePower) <= n.
			self assert: (theTruncatedRoot + 1 raisedTo: thePower) > n]]! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'dtl 12/May/2024, 1:35:00 pm (UTC)'!
testSinCosConsistency
	"sin and cos are tricky on very large numbers. Check for correct behavior."

	"Angles within a reasonable range"
	-1234567 to: 123456789 by: 113*100 do: [:angle | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001].

	"Large magnitude angles. results should be consistent"
	(1 to: 1000) inject: (2 raisedToInteger: 122) into: [ :angle :i | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001.
		angle asFloat successor asInteger].

	"Large negative angle magnitudes"
	(1 to: 1000) inject: (2 raisedToInteger: 122) negated into: [ :angle :i | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001.
		angle asFloat predecessor asInteger].! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'ul 25/Nov/2009, 7:01:00 am (UTC)'!
testSqrtFloor

	#(-1234567890123 -10 -5 -1) do: [ :each |
		self should: [ each sqrtFloor ] raise: Error ].
	#(
		0 1 2 3 4 5 10 16 30 160479924 386234481 501619156 524723498 580855366 766098594 834165249 1020363860 1042083924 1049218924
		1459774772895569 3050005981408238 4856589481837079 5650488387708463 7831037396100244) do: [ :each |
			self assert: each asFloat sqrt floor = each sqrtFloor ]
		! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 27/Dec/2009, 7:11:00 am (UTC)'!
testBitAt
	| trials bitSequence2 |

	self
		assert: ((1 to: 100) allSatisfy: [:i | (0 bitAt: i) = 0])
		description: 'all bits of zero are set to zero'.
	
	self
		assert: ((1 to: 100) allSatisfy: [:i | (-1 bitAt: i) = 1])
		description: 'In two complements, all bits of -1 are set to 1'.
		
	
	trials := #(
		'2r10010011'
		'2r11100100'
		'2r10000000'
		'2r0000101011011001'
		'2r1000101011011001'
		'2r0101010101011000'
		'2r0010011110110010'
		'2r0010011000000000'
		'2r00100111101100101000101011011001'
		'2r01110010011110110010100110101101'
		'2r10101011101011001010000010110110'
		'2r10101000000000000000000000000000'
		'2r0010101110101001110010100000101101100010011110110010100010101100'
		'2r1010101110101100101000001011011000100111101100101000101011011001'
		'2r1010101110101000000000000000000000000000000000000000000000000000').
	trials do: [:bitSequence | | aNumber |
		aNumber := Number readFrom: bitSequence.
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: (aNumber bitAt: i))].
		self assert: bitSequence2 = bitSequence].
	
	trials do: [:bitSequence | | bitInvert |
		bitInvert := -1 - (Number readFrom: bitSequence).
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: 1 - (bitInvert bitAt: i))].
		self assert: bitSequence2 = bitSequence description: '-1-x is similar to a bitInvert operation in two complement']! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'sd 5/Jun/2005, 12:43:00 pm (UTC)'!
testBitLogic  
	"This little suite of tests is designed to verify correct operation of most
	of Squeak's bit manipulation code, including two's complement
	representation of negative values.  It was written in a hurry and
	is probably lacking several important checks."

	"Shift 1 bit left then right and test for 1"
	"self run: #testBitLogic"
	| n |
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) = 1].

	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((-1 bitShift: i) bitShift: i negated) = -1].

	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i)].

	"Verify that (n bitAnd: n negated) = n for single bits"
	1 to: 100 do: [:i |  n := 1 bitShift: i. self assert: (n bitAnd: n negated) = n].

	"Verify that n negated = (n complemented + 1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i. 
					self assert: n negated = ((n bitXor: -1) + 1)].

	"Verify that (n + n complemented) = -1 for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: (n + (n bitXor: -1)) = -1].

	"Verify that n negated = (n complemented +1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: n negated = ((n bitXor: -1) + 1)].

	self assert: (-2 bitAnd: 16rFFFFFFFF) = 16rFFFFFFFE.! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 3/Oct/2018, 10:03:31 pm (UTC)'!
testBitReversal
	{ 0. 1. SmallInteger maxVal-1. SmallInteger maxVal. SmallInteger maxVal+1. (2 raisedTo: 64)-1091. (2 raisedTo: 64)-1090. (2 raisedTo: 64)-1 } do: 
		[ : fixture |
		| printedThenReversed reversedThenPrinted |
		printedThenReversed := (fixture printStringBase: 2 length: 64 padded: true) reversed.
		reversedThenPrinted := (fixture bitReverse: 64) printStringBase: 2 length: 64 padded: true.
		self assert: printedThenReversed = reversedThenPrinted ]! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 24/Jun/2020, 3:23:19 pm (UTC)'!
testBitReversalCorrectClass
	"This test was added due to a bug!!
	The problem only happened in 64 bit systems running with jitted Cog VM."

	| a b |
	a _ 16r944F245FDBB2B06.
	10 timesRepeat: [
		b _ (a bitReverse: 64) bitReverse: 64.
		self assert: a class = b class ]! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 8/Jul/2008, 6:47:00 am (UTC)'!
testHighBit
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| highBit shifted |
		highBit := 0.
		shifted := 1.
		[shifted > anInteger] whileFalse: [highBit := highBit+1. shifted := shifted bitShift: 1].
		self assert: anInteger highBit = highBit].! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 8/Jul/2008, 6:44:00 am (UTC)'!
testHighBitOfMagnitude
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| highBit shifted |
		highBit := 0.
		shifted := 1.
		[shifted > anInteger] whileFalse: [highBit := highBit+1. shifted := shifted bitShift: 1].
		self assert: anInteger highBitOfMagnitude = highBit.
		self assert: anInteger negated highBitOfMagnitude = highBit].! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 10/Dec/2018, 10:36:38 pm (UTC)'!
testLowBit
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| lowBit |
		lowBit := (anInteger respondsTo: #bitAt:)
			ifTrue: [(1 to: anInteger highBit) detect: [:bitIndex | (anInteger bitAt: bitIndex) ~= 0] ifNone: [0]]
			ifFalse: [(1 to: anInteger highBit) detect: [:bitIndex | (anInteger bitAnd: (1 bitShift: bitIndex-1)) ~= 0] ifNone: [0]].
		self assert: anInteger lowBit = lowBit.
		self assert: anInteger negated lowBit = lowBit].
    self assert: (LargePositiveInteger new: 0) lowBit = 0.
    self assert: (LargePositiveInteger new: 2) lowBit = 0! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 26/Jan/2008, 6:22:00 am (UTC)'!
testTwoComplementBitLogicWithCarry
	"This is non regression test for http://bugs.squeak.org/view.php?id=6874"
	
	"By property of two complement, following operation is:
	...111110000 this is -16
	...111101111 this is -16-1
	...111100000 this is -32, the result of bitAnd: on two complement
	
	This test used to fail with n=31 39 47.... because of bug 6874"
	
	self assert: ((2 to: 80) allSatisfy: [:n | ((2 raisedTo: n) negated bitAnd: (2 raisedTo: n) negated - 1) = (2 raisedTo: n + 1) negated]).! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'sd 5/Jun/2005, 12:44:00 pm (UTC)'!
testTwoComplementRightShift
	"self run: #testTwoComplementRightShift"

	| large small |
	small := 2 << 16.
	large := 2 << 32.	
	self assert: ((small negated bitShift: -1) ~= ((small + 1) negated bitShift: -1)
		== ((large negated bitShift: -1) ~= ((large + 1) negated bitShift: -1))).
		
     self assert: ((small bitShift: -1) ~= (small + 1 bitShift: -1)
		== ((large bitShift: -1) ~= (large + 1 bitShift: -1))).! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'mga 11/May/2006, 7:41:00 pm (UTC)'!
testCrossSumBase
	"self run: #testCrossSumBase"

	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 10]) asArray = 
		#(2 10 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10 2)).
	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 2]) asArray = 
		#(2 3 2 2 1 4 3 3 2 3 2 2 1 3 2 2 1 2 1 1 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2)).
	self should: [10 crossSumBase: 1] raise: AssertionFailure! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 9/Jan/2013, 6:11:00 am (UTC)'!
testIntegerDivision
	| suite |
	suite := #( 1 2 5 1000 123456798  111222333444555 987654321098765432109876 ).
	suite := suite , (suite collect: [:e | e negated]).
	suite do: [:a |
		suite do: [:b |
			| q r |
			q := a // b.
			r := a \\ b.
			self assert: b * q + r = a.
			self assert: r abs < b abs.
			self assert: (r isZero or: [b negative = r negative])]].! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 16/Jan/2013, 10:38:00 pm (UTC)'!
testMontgomeryMultiplication
	| a m mInv |
	m := 15485863.
	mInv := m montgomeryDigitBase - ((m bitAnd: m montgomeryDigitMax) reciprocalModulo: m montgomeryDigitBase).
	a := (m montgomeryDigitBase raisedTo: m montgomeryNumberOfDigits) \\ m.
	#(483933 3871465 8951195) do: [:s |
		(s montgomeryTimes: a modulo: m mInvModB: mInv) ifNotNil: [:s1 |
			| s2 sa ssa |
			self assert: s = s1.
			sa := s montgomeryTimes: (a * a \\ m) modulo: m mInvModB: mInv.
			self assert: sa = (s * a \\ m).
			ssa := sa montgomeryTimes: sa modulo: m mInvModB: mInv.
			self assert: ssa = (s * s * a \\ m).
			s2 := ssa montgomeryTimes: 1 modulo: m mInvModB: mInv.
			self assert: s2 = (s * s \\ m)]].! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 9/Jan/2013, 6:11:00 am (UTC)'!
testQuoRem
	| suite |
	suite := #( 1 2 5 1000 123456798 111222333444555 987654321098765432109876 ).
	suite := suite , (suite collect: [:e | e negated]).
	suite do: [:a |
		suite do: [:b |
			| q r |
			q := a quo: b.
			r := a rem: b.
			self assert: b * q + r = a.
			self assert: r abs < b abs.
			self assert: (r isZero or: [a negative = r negative])]].! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 5/Jun/2011, 12:46:00 am (UTC)'!
testRaisedToModulo
	#(301 2047) do: [:m |
		1 to: m - 1 by: (m // 30) do: [:x |
			11 to: m - 1 by: (m // 40) do: [:y |
				self assert: (x raisedTo: y) \\ m = (x raisedTo: y modulo: m)]]].
	self assert: (8951195 raisedTo: 7742931 modulo: 15485863) = 15485862.! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 21/Apr/2003, 8:17:00 pm (UTC)'!
testEven
	
	self deny: (1073741825 even).
	self assert: (1073741824  even).
	! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'jpb 3/Aug/2019, 3:55:59 am (UTC)'!
testIsInteger
  self assert: 0 isInteger.
  self assert: (0 is: #Integer).
	self assert: (1 is: #Integer).
	self assert: (1 is: #Number).
	
	self assert: (10000 is: #Integer).
	self assert: (10000 is: #Number).! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'jmv 24/May/2024, 6:25:05 pm (UTC)'!
testIsPowerOfTwo

	self deny: (0 isPowerOfTwo).
	self assert: (1 isPowerOfTwo).
	self assert: (2 isPowerOfTwo).
	self deny:  (3 isPowerOfTwo).
	self assert: (4 isPowerOfTwo).
	! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'nice 26/Jan/2008, 2:51:00 am (UTC)'!
testIsPowerOfTwoM6873
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6873"

	self deny: ((1 to: 80) anySatisfy: [:n | (2 raisedTo: n) negated isPowerOfTwo])
		description: 'A negative integer cannot be a power of two'.! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'ul 25/Nov/2009, 6:51:00 am (UTC)'!
testIsPrime

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743) do: [ :each |
		self assert: each isPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isPrime ].! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'ul 25/Nov/2009, 6:49:00 am (UTC)'!
testIsProbablyPrime

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isProbablyPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743 29996224275833) do: [ :each |
		self assert: each isProbablyPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isProbablyPrime ].! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 12/Feb/2006, 6:36:00 pm (UTC)'!
testLargePrimesUpTo

	| nn | 
	nn := (2 raisedTo: 17) - 1. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
	
	
! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 12/Feb/2006, 6:36:00 pm (UTC)'!
testPrimesUpTo

	| primes nn|
	primes := Integer primesUpTo: 100.
	self assert: primes = #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97).
	
	"upTo: semantics means 'non-inclusive'"
	primes := Integer primesUpTo: 5.
	self assert: primes = #(2 3).
	
	"this test is green for nn>25000, see #testLargePrimesUpTo"
	nn := 5. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'eem 16/Jul/2014, 7:29:00 pm (UTC)'!
testRange
	self assert: SmallInteger maxVal class equals: SmallInteger.
	self assert: (SmallInteger maxVal + 1) class equals: LargePositiveInteger.
	self assert: SmallInteger minVal class equals: SmallInteger.
	self assert: (SmallInteger minVal - 1) class equals: LargeNegativeInteger! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'sd 5/Jun/2005, 12:46:00 pm (UTC)'!
testDifferentBases
	"self run: #testDifferentBases"
	"| value |
	2 to: 36 do: [:each|
		value := 0.
		1 to: each-1 do: [:n| value := value + (n * (each raisedToInteger: n))].
		value := value negated.
		Transcript tab; show: 'self assert: (', value printString, ' printStringBase: ', each printString, ') = ''', (value printStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' radix: ', each printString, ') = ''', (value radix: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' printStringHex = ''', (value printStringBase: 16), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' storeStringBase: ', each printString, ') = ''', (value storeStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' storeStringHex = ''', (value storeStringBase: 16), '''.'; cr.


].
	"

	self assert: 2r10 = 2.
	self assert: 3r210 = 21.
	self assert: 4r3210 = 228.
	self assert: 5r43210 = 2930.
	self assert: 6r543210 = 44790.
	self assert: 7r6543210 = 800667.
	self assert: 8r76543210 = 16434824.
	self assert: 9r876543210 = 381367044.
	self assert: 10r9876543210 = 9876543210.
	self assert: 11rA9876543210 = 282458553905.
	self assert: 12rBA9876543210 = 8842413667692.
	self assert: 13rCBA9876543210 = 300771807240918.
	self assert: 14rDCBA9876543210 = 11046255305880158.
	self assert: 15rEDCBA9876543210 = 435659737878916215.
	self assert: 16rFEDCBA9876543210 = 18364758544493064720.
	self assert: 17rGFEDCBA9876543210 = 824008854613343261192.
	self assert: 18rHGFEDCBA9876543210 = 39210261334551566857170.
	self assert: 19rIHGFEDCBA9876543210 = 1972313422155189164466189.
	self assert: 20rJIHGFEDCBA9876543210 = 104567135734072022160664820.
	self assert: 21rKJIHGFEDCBA9876543210 = 5827980550840017565077671610.
	self assert: 22rLKJIHGFEDCBA9876543210 = 340653664490377789692799452102.
	self assert: 23rMLKJIHGFEDCBA9876543210 = 20837326537038308910317109288851.
	self assert: 24rNMLKJIHGFEDCBA9876543210 = 1331214537196502869015340298036888.
	self assert: 25rONMLKJIHGFEDCBA9876543210 = 88663644327703473714387251271141900.
	self assert: 26rPONMLKJIHGFEDCBA9876543210 = 6146269788878825859099399609538763450.
	self assert: 27rQPONMLKJIHGFEDCBA9876543210 = 442770531899482980347734468443677777577.
	self assert: 28rRQPONMLKJIHGFEDCBA9876543210 = 33100056003358651440264672384704297711484.
	self assert: 29rSRQPONMLKJIHGFEDCBA9876543210 = 2564411043271974895869785066497940850811934.
	self assert: 30rTSRQPONMLKJIHGFEDCBA9876543210 = 205646315052919334126040428061831153388822830.
	self assert: 31rUTSRQPONMLKJIHGFEDCBA9876543210 = 17050208381689099029767742314582582184093573615.
	self assert: 32rVUTSRQPONMLKJIHGFEDCBA9876543210 = 1459980823972598128486511383358617792788444579872.
	self assert: 33rWVUTSRQPONMLKJIHGFEDCBA9876543210 = 128983956064237823710866404905431464703849549412368.
	self assert: 34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 11745843093701610854378775891116314824081102660800418.
	self assert: 35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 1101553773143634726491620528194292510495517905608180485.
	self assert: 36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 106300512100105327644605138221229898724869759421181854980.

	self assert: -2r10 = -2.
	self assert: -3r210 = -21.
	self assert: -4r3210 = -228.
	self assert: -5r43210 = -2930.
	self assert: -6r543210 = -44790.
	self assert: -7r6543210 = -800667.
	self assert: -8r76543210 = -16434824.
	self assert: -9r876543210 = -381367044.
	self assert: -10r9876543210 = -9876543210.
	self assert: -11rA9876543210 = -282458553905.
	self assert: -12rBA9876543210 = -8842413667692.
	self assert: -13rCBA9876543210 = -300771807240918.
	self assert: -14rDCBA9876543210 = -11046255305880158.
	self assert: -15rEDCBA9876543210 = -435659737878916215.
	self assert: -16rFEDCBA9876543210 = -18364758544493064720.
	self assert: -17rGFEDCBA9876543210 = -824008854613343261192.
	self assert: -18rHGFEDCBA9876543210 = -39210261334551566857170.
	self assert: -19rIHGFEDCBA9876543210 = -1972313422155189164466189.
	self assert: -20rJIHGFEDCBA9876543210 = -104567135734072022160664820.
	self assert: -21rKJIHGFEDCBA9876543210 = -5827980550840017565077671610.
	self assert: -22rLKJIHGFEDCBA9876543210 = -340653664490377789692799452102.
	self assert: -23rMLKJIHGFEDCBA9876543210 = -20837326537038308910317109288851.
	self assert: -24rNMLKJIHGFEDCBA9876543210 = -1331214537196502869015340298036888.
	self assert: -25rONMLKJIHGFEDCBA9876543210 = -88663644327703473714387251271141900.
	self assert: -26rPONMLKJIHGFEDCBA9876543210 = -6146269788878825859099399609538763450.
	self assert: -27rQPONMLKJIHGFEDCBA9876543210 = -442770531899482980347734468443677777577.
	self assert: -28rRQPONMLKJIHGFEDCBA9876543210 = -33100056003358651440264672384704297711484.
	self assert: -29rSRQPONMLKJIHGFEDCBA9876543210 = -2564411043271974895869785066497940850811934.
	self assert: -30rTSRQPONMLKJIHGFEDCBA9876543210 = -205646315052919334126040428061831153388822830.
	self assert: -31rUTSRQPONMLKJIHGFEDCBA9876543210 = -17050208381689099029767742314582582184093573615.
	self assert: -32rVUTSRQPONMLKJIHGFEDCBA9876543210 = -1459980823972598128486511383358617792788444579872.
	self assert: -33rWVUTSRQPONMLKJIHGFEDCBA9876543210 = -128983956064237823710866404905431464703849549412368.
	self assert: -34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -11745843093701610854378775891116314824081102660800418.
	self assert: -35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -1101553773143634726491620528194292510495517905608180485.
	self assert: -36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -106300512100105327644605138221229898724869759421181854980.! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 1/Sep/2010, 5:56:00 pm (UTC)'!
testIntegerReadFrom
	self assert: (Integer readFrom: '123' readStream base: 10) = 123.
	self assert: (Integer readFrom: '-123' readStream base: 10) = -123.
	self should: [Integer readFrom: 'abc' readStream base: 10] raise: Error.
	self should: [Integer readFrom: 'D12' readStream base: 10] raise: Error.
	self assert: (Integer readFrom: '1two3' readStream base: 10) = 1.
! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 8/Aug/2024, 4:28:09 pm (UTC)'!
testNew
	self should: [Integer new] raise: TestResult exError. ! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 1/Sep/2010, 5:26:00 pm (UTC)'!
testReadFrom
	"Ensure remaining characters in a stream are not lost when parsing an integer."

	| rs i s |
	rs := ReadStream on: '123s could be confused with a ScaledDecimal'.
	i := Number readFrom: rs.
	self assert: i isInteger.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' = s.
	
	"Modified for Cuis. Classic number parsing considers this to be an integer."
	rs := ReadStream on: '123.s could be confused with a ScaledDecimal'.
	i := Number readFrom: rs.
	self assert: i isInteger.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' = s
! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 1/Sep/2010, 5:45:00 pm (UTC)'!
testStringAsNumber
	"This covers parsing in Number>>readFrom:
	Trailing decimal points should be ignored."

	self assert: ('123' asNumber isInteger).
	self assert: ('123' asNumber = 123).
	self assert: ('-123' asNumber isInteger).
	self assert: ('-123' asNumber = -123).
	self assert: ('123.' asNumber) isInteger.
	self assert: ('123.' asNumber = 123).
	self assert: ('-123.' asNumber) isInteger.
	self assert: ('-123.' asNumber = -123).
	self assert: ('123This is not to be read' asNumber isInteger).
	self assert: ('123This is not to be read' asNumber = 123).
	self assert: ('123s could be confused with a ScaledDecimal' asNumber isInteger).
	self assert: ('123s could be confused with a ScaledDecimal' asNumber = 123).
	self assert: ('123e could be confused with a Float' asNumber isInteger).
	self assert: ('123e could be confused with a Float' asNumber = 123).
! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:53:00 pm (UTC)'!
testDivLnLn

	self assert: (-42391158275216203514294433200 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433201 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 div: -14130386091738734504764811067) = 4.
	self assert: (-8727963568087712425891397479476727340041448 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: -79766443076872509863361) = 109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:53:00 pm (UTC)'!
testDivLnLp

	self assert: (-42391158275216203514294433200 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433201 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 div: 14130386091738734504764811067) = -4.
	self assert: (-8727963568087712425891397479476727340041448 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: 79766443076872509863361) = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:54:00 pm (UTC)'!
testDivLnSn

	self assert: (-42391158275216203514294433201 div: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: -3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivLnSp

	self assert: (-42391158275216203514294433201 div: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: 3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:54:00 pm (UTC)'!
testDivLpLn

	self assert: (42391158275216203514294433200 div: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 div: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 div: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 div: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: -79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivLpLp

	self assert: (42391158275216203514294433200 div: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 div: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 div: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 div: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: 79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:55:00 pm (UTC)'!
testDivLpSn

	self assert: (42391158275216203514294433201 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: -3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivLpSp

	self assert: (42391158275216203514294433201 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: 3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:55:00 pm (UTC)'!
testDivSnLn

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (-1 div: -42391158275216203514294433201) = 1.
	self assert: (-14348907 div: -42391158275216203514294433201) = 1! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivSnLp

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (-1 div: 42391158275216203514294433201) = -1.
	self assert: (-14348907 div: 42391158275216203514294433201) = -1! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:56:00 pm (UTC)'!
testDivSnSn

	self assert: (0 div: -3) = 0.
	self assert: (-1 div: -3) = 1.
	self assert: (-9 div: -3) = 3.
	self assert: (-10 div: -3) = 4.
	self assert: (-11 div: -3) = 4.
	self assert: (-12 div: -3) = 4.
	self assert: (-13 div: -3) = 5! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivSnSp

	self assert: (0 div: 3) = 0.
	self assert: (-1 div: 3) = -1.
	self assert: (-9 div: 3) = -3.
	self assert: (-10 div: 3) = -4.
	self assert: (-11 div: 3) = -4.
	self assert: (-12 div: 3) = -4.
	self assert: (-13 div: 3) = -5! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:55:00 pm (UTC)'!
testDivSpLn

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (1 div: -42391158275216203514294433201) = 0.
	self assert: (14348907 div: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivSpLp

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (1 div: 42391158275216203514294433201) = 0.
	self assert: (14348907 div: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:54:00 pm (UTC)'!
testDivSpSn

	self assert: (0 div: -3) = 0.
	self assert: (1 div: -3) = 0.
	self assert: (9 div: -3) = -3.
	self assert: (10 div: -3) = -3.
	self assert: (11 div: -3) = -3.
	self assert: (12 div: -3) = -4.
	self assert: (13 div: -3) = -4! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testDivSpSp

	self assert: (0 div: 3) = 0.
	self assert: (1 div: 3) = 0.
	self assert: (9 div: 3) = 3.
	self assert: (10 div: 3) = 3.
	self assert: (11 div: 3) = 3.
	self assert: (12 div: 3) = 4.
	self assert: (13 div: 3) = 4! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 10/Jan/2014, 3:39:00 am (UTC)'!
testIntegerPadding
	"self run: #testIntegerPadding"

	self assert: (1 printStringBase: 10 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: false) = ' 1'.
	self assert: (1024 printStringBase: 10 length: 19 padded: false) = '               1024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: false) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: false) =  ' 1024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: false) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: false) =  '              -1024'.

	self assert: (1 printStringBase: 10 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: true) = '01'.
	self assert: (1024 printStringBase: 10 length: 19 padded: true) = '0000000000000001024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: true) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: true) =  '01024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: true) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: true) =  '-000000000000001024'.

	self assert: (1 printStringBase: 16 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: false) = ' 1'.
	self assert: (2047 printStringBase: 16 length: 19 padded: false) =  '                7FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: false) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: false) =  ' 7FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: false) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: false) =  '               -7FF'.

	self assert: (1 printStringBase: 16 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: true) = '01'.
	self assert: (2047 printStringBase: 16 length: 19 padded: true) =  '00000000000000007FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: true) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: true) =  '07FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: true) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: true) =  '-0000000000000007FF'! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 10/Jan/2014, 2:54:00 am (UTC)'!
testNegativeIntegerPrinting
	"self run: #testnegativeIntegerPrinting"

	self assert: (-2 printStringBase: 2) = '-10'.
	self assert: -2 printStringHex = '-2'.
	self assert: (-2 storeStringBase: 2) = '-2r10'.
	self assert: -2 storeStringHex = '-16r2'.
	self assert: (-21 printStringBase: 3) = '-210'.
	self assert: -21 printStringHex = '-15'.
	self assert: (-21 storeStringBase: 3) = '-3r210'.
	self assert: -21 storeStringHex = '-16r15'.
	self assert: (-228 printStringBase: 4) = '-3210'.
	self assert: -228 printStringHex = '-E4'.
	self assert: (-228 storeStringBase: 4) = '-4r3210'.
	self assert: -228 storeStringHex = '-16rE4'.
	self assert: (-2930 printStringBase: 5) = '-43210'.
	self assert: -2930 printStringHex = '-B72'.
	self assert: (-2930 storeStringBase: 5) = '-5r43210'.
	self assert: -2930 storeStringHex = '-16rB72'.
	self assert: (-44790 printStringBase: 6) = '-543210'.
	self assert: -44790 printStringHex = '-AEF6'.
	self assert: (-44790 storeStringBase: 6) = '-6r543210'.
	self assert: -44790 storeStringHex = '-16rAEF6'.
	self assert: (-800667 printStringBase: 7) = '-6543210'.
	self assert: -800667 printStringHex = '-C379B'.
	self assert: (-800667 storeStringBase: 7) = '-7r6543210'.
	self assert: -800667 storeStringHex = '-16rC379B'.
	self assert: (-16434824 printStringBase: 8) = '-76543210'.
	self assert: -16434824 printStringHex = '-FAC688'.
	self assert: (-16434824 storeStringBase: 8) = '-8r76543210'.
	self assert: -16434824 storeStringHex = '-16rFAC688'.
	self assert: (-381367044 printStringBase: 9) = '-876543210'.
	self assert: -381367044 printStringHex = '-16BB3304'.
	self assert: (-381367044 storeStringBase: 9) = '-9r876543210'.
	self assert: -381367044 storeStringHex = '-16r16BB3304'.
	self assert: (-9876543210 printStringBase: 10) = '-9876543210'.
	self assert: -9876543210 printStringHex = '-24CB016EA'.
	self assert: (-9876543210 storeStringBase: 10) = '-9876543210'.
	self assert: -9876543210 storeStringHex = '-16r24CB016EA'.
	self assert: (-282458553905 printStringBase: 11) = '-A9876543210'.
	self assert: -282458553905 printStringHex = '-41C3D77E31'.
	self assert: (-282458553905 storeStringBase: 11) = '-11rA9876543210'.
	self assert: -282458553905 storeStringHex = '-16r41C3D77E31'.
	self assert: (-8842413667692 printStringBase: 12) = '-BA9876543210'.
	self assert: -8842413667692 printStringHex = '-80AC8ECF56C'.
	self assert: (-8842413667692 storeStringBase: 12) = '-12rBA9876543210'.
	self assert: -8842413667692 storeStringHex = '-16r80AC8ECF56C'.
	self assert: (-300771807240918 printStringBase: 13) = '-CBA9876543210'.
	self assert: -300771807240918 printStringHex = '-1118CE4BAA2D6'.
	self assert: (-300771807240918 storeStringBase: 13) = '-13rCBA9876543210'.
	self assert: -300771807240918 storeStringHex = '-16r1118CE4BAA2D6'.
	self assert: (-11046255305880158 printStringBase: 14) = '-DCBA9876543210'.
	self assert: -11046255305880158 printStringHex = '-273E82BB9AF25E'.
	self assert: (-11046255305880158 storeStringBase: 14) = '-14rDCBA9876543210'.
	self assert: -11046255305880158 storeStringHex = '-16r273E82BB9AF25E'.
	self assert: (-435659737878916215 printStringBase: 15) = '-EDCBA9876543210'.
	self assert: -435659737878916215 printStringHex = '-60BC6392F366C77'.
	self assert: (-435659737878916215 storeStringBase: 15) = '-15rEDCBA9876543210'.
	self assert: -435659737878916215 storeStringHex = '-16r60BC6392F366C77'.
	self assert: (-18364758544493064720 printStringBase: 16) = '-FEDCBA9876543210'.
	self assert: -18364758544493064720 printStringHex = '-FEDCBA9876543210'.
	self assert: (-18364758544493064720 storeStringBase: 16) = '-16rFEDCBA9876543210'.
	self assert: -18364758544493064720 storeStringHex = '-16rFEDCBA9876543210'.
	self assert: (-824008854613343261192 printStringBase: 17) = '-GFEDCBA9876543210'.
	self assert: -824008854613343261192 printStringHex = '-2CAB6B877C1CD2D208'.
	self assert: (-824008854613343261192 storeStringBase: 17) = '-17rGFEDCBA9876543210'.
	self assert: -824008854613343261192 storeStringHex = '-16r2CAB6B877C1CD2D208'.
	self assert: (-39210261334551566857170 printStringBase: 18) = '-HGFEDCBA9876543210'.
	self assert: -39210261334551566857170 printStringHex = '-84D97AFCAE81415B3D2'.
	self assert: (-39210261334551566857170 storeStringBase: 18) = '-18rHGFEDCBA9876543210'.
	self assert: -39210261334551566857170 storeStringHex = '-16r84D97AFCAE81415B3D2'.
	self assert: (-1972313422155189164466189 printStringBase: 19) = '-IHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 printStringHex = '-1A1A75329C5C6FC00600D'.
	self assert: (-1972313422155189164466189 storeStringBase: 19) = '-19rIHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 storeStringHex = '-16r1A1A75329C5C6FC00600D'.
	self assert: (-104567135734072022160664820 printStringBase: 20) = '-JIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 printStringHex = '-567EF3C9636D242A8C68F4'.
	self assert: (-104567135734072022160664820 storeStringBase: 20) = '-20rJIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 storeStringHex = '-16r567EF3C9636D242A8C68F4'.
	self assert: (-5827980550840017565077671610 printStringBase: 21) = '-KJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 printStringHex = '-12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-5827980550840017565077671610 storeStringBase: 21) = '-21rKJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 storeStringHex = '-16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-340653664490377789692799452102 printStringBase: 22) = '-LKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 printStringHex = '-44CB61B5B47E1A5D8F88583C6'.
	self assert: (-340653664490377789692799452102 storeStringBase: 22) = '-22rLKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 storeStringHex = '-16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (-20837326537038308910317109288851 printStringBase: 23) = '-MLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 printStringHex = '-1070108876456E0EF115B389F93'.
	self assert: (-20837326537038308910317109288851 storeStringBase: 23) = '-23rMLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 storeStringHex = '-16r1070108876456E0EF115B389F93'.
	self assert: (-1331214537196502869015340298036888 printStringBase: 24) = '-NMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 printStringHex = '-41A24A285154B026B6ED206C6698'.
	self assert: (-1331214537196502869015340298036888 storeStringBase: 24) = '-24rNMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 storeStringHex = '-16r41A24A285154B026B6ED206C6698'.
	self assert: (-88663644327703473714387251271141900 printStringBase: 25) = '-ONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 printStringHex = '-111374860A2C6CEBE5999630398A0C'.
	self assert: (-88663644327703473714387251271141900 storeStringBase: 25) = '-25rONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 storeStringHex = '-16r111374860A2C6CEBE5999630398A0C'.
	self assert: (-6146269788878825859099399609538763450 printStringBase: 26) = '-PONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 printStringHex = '-49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-6146269788878825859099399609538763450 storeStringBase: 26) = '-26rPONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 storeStringHex = '-16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-442770531899482980347734468443677777577 printStringBase: 27) = '-QPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 printStringHex = '-14D1A80A997343640C1145A073731DEA9'.
	self assert: (-442770531899482980347734468443677777577 storeStringBase: 27) = '-27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 storeStringHex = '-16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (-33100056003358651440264672384704297711484 printStringBase: 28) = '-RQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 printStringHex = '-6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-33100056003358651440264672384704297711484 storeStringBase: 28) = '-28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 storeStringHex = '-16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-2564411043271974895869785066497940850811934 printStringBase: 29) = '-SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 printStringHex = '-1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-2564411043271974895869785066497940850811934 storeStringBase: 29) = '-29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 storeStringHex = '-16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-205646315052919334126040428061831153388822830 printStringBase: 30) = '-TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 printStringHex = '-938B4343B54B550989989D02998718FFB212E'.
	self assert: (-205646315052919334126040428061831153388822830 storeStringBase: 30) = '-30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 storeStringHex = '-16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (-17050208381689099029767742314582582184093573615 printStringBase: 31) = '-UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 printStringHex = '-2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-17050208381689099029767742314582582184093573615 storeStringBase: 31) = '-31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 storeStringHex = '-16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-1459980823972598128486511383358617792788444579872 printStringBase: 32) = '-VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 printStringHex = '-FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-1459980823972598128486511383358617792788444579872 storeStringBase: 32) = '-32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 storeStringHex = '-16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-128983956064237823710866404905431464703849549412368 printStringBase: 33) = '-WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 printStringHex = '-584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-128983956064237823710866404905431464703849549412368 storeStringBase: 33) = '-33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 storeStringHex = '-16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-11745843093701610854378775891116314824081102660800418 printStringBase: 34) = '-XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 printStringHex = '-1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-11745843093701610854378775891116314824081102660800418 storeStringBase: 34) = '-34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 storeStringHex = '-16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 printStringBase: 35) = '-YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 printStringHex = '-B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) = '-35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 storeStringHex = '-16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 printStringBase: 36) = '-ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 printStringHex = '-455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) = '-36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 storeStringHex = '-16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'nice 16/Feb/2008, 2:23:00 am (UTC)'!
testNumberOfDigits
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN + 1 numberOfDigitsInBase: b) = (n+1).
			
			self assert: (bRaisedToN negated + 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN negated numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN negated - 1 numberOfDigitsInBase: b) = (n+1).]].
! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 10/Jan/2014, 3:00:00 am (UTC)'!
testPositiveIntegerPrinting
	"self run: #testPositiveIntegerPrinting"

	self assert: 0 printString = '0'.
	self assert: 0 printStringHex = '0'.
	self assert: 0 storeStringHex = '16r0'.
	self assert: (2 printStringBase: 2) = '10'.
	self assert: 2 printStringHex = '2'.
	self assert: (2 storeStringBase: 2) = '2r10'.
	self assert: 2 storeStringHex = '16r2'.
	self assert: (21 printStringBase: 3) = '210'.
	self assert: 21 printStringHex = '15'.
	self assert: (21 storeStringBase: 3) = '3r210'.
	self assert: 21 storeStringHex = '16r15'.
	self assert: (228 printStringBase: 4) = '3210'.
	self assert: 228 printStringHex = 'E4'.
	self assert: (228 storeStringBase: 4) = '4r3210'.
	self assert: 228 storeStringHex = '16rE4'.
	self assert: (2930 printStringBase: 5) = '43210'.
	self assert: 2930 printStringHex = 'B72'.
	self assert: (2930 storeStringBase: 5) = '5r43210'.
	self assert: 2930 storeStringHex = '16rB72'.
	self assert: (44790 printStringBase: 6) = '543210'.
	self assert: 44790 printStringHex = 'AEF6'.
	self assert: (44790 storeStringBase: 6) = '6r543210'.
	self assert: 44790 storeStringHex = '16rAEF6'.
	self assert: (800667 printStringBase: 7) = '6543210'.
	self assert: 800667 printStringHex = 'C379B'.
	self assert: (800667 storeStringBase: 7) = '7r6543210'.
	self assert: 800667 storeStringHex = '16rC379B'.
	self assert: (16434824 printStringBase: 8) = '76543210'.
	self assert: 16434824 printStringHex = 'FAC688'.
	self assert: (16434824 storeStringBase: 8) = '8r76543210'.
	self assert: 16434824 storeStringHex = '16rFAC688'.
	self assert: (381367044 printStringBase: 9) = '876543210'.
	self assert: 381367044 printStringHex = '16BB3304'.
	self assert: (381367044 storeStringBase: 9) = '9r876543210'.
	self assert: 381367044 storeStringHex = '16r16BB3304'.
	self assert: (9876543210 printStringBase: 10) = '9876543210'.
	self assert: 9876543210 printStringHex = '24CB016EA'.
	self assert: (9876543210 storeStringBase: 10) = '9876543210'.
	self assert: 9876543210 storeStringHex = '16r24CB016EA'.
	self assert: (282458553905 printStringBase: 11) = 'A9876543210'.
	self assert: 282458553905 printStringHex = '41C3D77E31'.
	self assert: (282458553905 storeStringBase: 11) = '11rA9876543210'.
	self assert: 282458553905 storeStringHex = '16r41C3D77E31'.
	self assert: (8842413667692 printStringBase: 12) = 'BA9876543210'.
	self assert: 8842413667692 printStringHex = '80AC8ECF56C'.
	self assert: (8842413667692 storeStringBase: 12) = '12rBA9876543210'.
	self assert: 8842413667692 storeStringHex = '16r80AC8ECF56C'.
	self assert: (300771807240918 printStringBase: 13) = 'CBA9876543210'.
	self assert: 300771807240918 printStringHex = '1118CE4BAA2D6'.
	self assert: (300771807240918 storeStringBase: 13) = '13rCBA9876543210'.
	self assert: 300771807240918 storeStringHex = '16r1118CE4BAA2D6'.
	self assert: (11046255305880158 printStringBase: 14) = 'DCBA9876543210'.
	self assert: 11046255305880158 printStringHex = '273E82BB9AF25E'.
	self assert: (11046255305880158 storeStringBase: 14) = '14rDCBA9876543210'.
	self assert: 11046255305880158 storeStringHex = '16r273E82BB9AF25E'.
	self assert: (435659737878916215 printStringBase: 15) = 'EDCBA9876543210'.
	self assert: 435659737878916215 printStringHex = '60BC6392F366C77'.
	self assert: (435659737878916215 storeStringBase: 15) = '15rEDCBA9876543210'.
	self assert: 435659737878916215 storeStringHex = '16r60BC6392F366C77'.
	self assert: (18364758544493064720 printStringBase: 16) = 'FEDCBA9876543210'.
	self assert: 18364758544493064720 printStringHex = 'FEDCBA9876543210'.
	self assert: (18364758544493064720 storeStringBase: 16) = '16rFEDCBA9876543210'.
	self assert: 18364758544493064720 storeStringHex = '16rFEDCBA9876543210'.
	self assert: (824008854613343261192 printStringBase: 17) = 'GFEDCBA9876543210'.
	self assert: 824008854613343261192 printStringHex = '2CAB6B877C1CD2D208'.
	self assert: (824008854613343261192 storeStringBase: 17) = '17rGFEDCBA9876543210'.
	self assert: 824008854613343261192 storeStringHex = '16r2CAB6B877C1CD2D208'.
	self assert: (39210261334551566857170 printStringBase: 18) = 'HGFEDCBA9876543210'.
	self assert: 39210261334551566857170 printStringHex = '84D97AFCAE81415B3D2'.
	self assert: (39210261334551566857170 storeStringBase: 18) = '18rHGFEDCBA9876543210'.
	self assert: 39210261334551566857170 storeStringHex = '16r84D97AFCAE81415B3D2'.
	self assert: (1972313422155189164466189 printStringBase: 19) = 'IHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 printStringHex = '1A1A75329C5C6FC00600D'.
	self assert: (1972313422155189164466189 storeStringBase: 19) = '19rIHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 storeStringHex = '16r1A1A75329C5C6FC00600D'.
	self assert: (104567135734072022160664820 printStringBase: 20) = 'JIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 printStringHex = '567EF3C9636D242A8C68F4'.
	self assert: (104567135734072022160664820 storeStringBase: 20) = '20rJIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 storeStringHex = '16r567EF3C9636D242A8C68F4'.
	self assert: (5827980550840017565077671610 printStringBase: 21) = 'KJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 printStringHex = '12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (5827980550840017565077671610 storeStringBase: 21) = '21rKJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 storeStringHex = '16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (340653664490377789692799452102 printStringBase: 22) = 'LKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 printStringHex = '44CB61B5B47E1A5D8F88583C6'.
	self assert: (340653664490377789692799452102 storeStringBase: 22) = '22rLKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 storeStringHex = '16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (20837326537038308910317109288851 printStringBase: 23) = 'MLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 printStringHex = '1070108876456E0EF115B389F93'.
	self assert: (20837326537038308910317109288851 storeStringBase: 23) = '23rMLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 storeStringHex = '16r1070108876456E0EF115B389F93'.
	self assert: (1331214537196502869015340298036888 printStringBase: 24) = 'NMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 printStringHex = '41A24A285154B026B6ED206C6698'.
	self assert: (1331214537196502869015340298036888 storeStringBase: 24) = '24rNMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 storeStringHex = '16r41A24A285154B026B6ED206C6698'.
	self assert: (88663644327703473714387251271141900 printStringBase: 25) = 'ONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 printStringHex = '111374860A2C6CEBE5999630398A0C'.
	self assert: (88663644327703473714387251271141900 storeStringBase: 25) = '25rONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 storeStringHex = '16r111374860A2C6CEBE5999630398A0C'.
	self assert: (6146269788878825859099399609538763450 printStringBase: 26) = 'PONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 printStringHex = '49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (6146269788878825859099399609538763450 storeStringBase: 26) = '26rPONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 storeStringHex = '16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (442770531899482980347734468443677777577 printStringBase: 27) = 'QPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 printStringHex = '14D1A80A997343640C1145A073731DEA9'.
	self assert: (442770531899482980347734468443677777577 storeStringBase: 27) = '27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 storeStringHex = '16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (33100056003358651440264672384704297711484 printStringBase: 28) = 'RQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 printStringHex = '6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (33100056003358651440264672384704297711484 storeStringBase: 28) = '28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 storeStringHex = '16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (2564411043271974895869785066497940850811934 printStringBase: 29) = 'SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 printStringHex = '1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (2564411043271974895869785066497940850811934 storeStringBase: 29) = '29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 storeStringHex = '16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (205646315052919334126040428061831153388822830 printStringBase: 30) = 'TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 printStringHex = '938B4343B54B550989989D02998718FFB212E'.
	self assert: (205646315052919334126040428061831153388822830 storeStringBase: 30) = '30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 storeStringHex = '16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (17050208381689099029767742314582582184093573615 printStringBase: 31) = 'UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 printStringHex = '2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (17050208381689099029767742314582582184093573615 storeStringBase: 31) = '31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 storeStringHex = '16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (1459980823972598128486511383358617792788444579872 printStringBase: 32) = 'VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 printStringHex = 'FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (1459980823972598128486511383358617792788444579872 storeStringBase: 32) = '32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 storeStringHex = '16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (128983956064237823710866404905431464703849549412368 printStringBase: 33) = 'WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 printStringHex = '584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (128983956064237823710866404905431464703849549412368 storeStringBase: 33) = '33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 storeStringHex = '16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (11745843093701610854378775891116314824081102660800418 printStringBase: 34) = 'XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 printStringHex = '1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (11745843093701610854378775891116314824081102660800418 storeStringBase: 34) = '34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 storeStringHex = '16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (1101553773143634726491620528194292510495517905608180485 printStringBase: 35) = 'YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 printStringHex = 'B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) = '35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 storeStringHex = '16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (106300512100105327644605138221229898724869759421181854980 printStringBase: 36) = 'ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 printStringHex = '455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) = '36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 storeStringHex = '16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 3/Oct/2018, 10:05:41 pm (UTC)'!
testPrintOnBaseShowRadix

	self assert: (123 printStringRadix: 10) = '10r123'.
	self assert: (123 printStringRadix: 8) = '8r173'.! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'nice 16/Feb/2008, 2:31:00 am (UTC)'!
testPrintStringBase
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 printStringBase: b) = (String new: n withAll: (Character digitValue: b-1)).
			self assert: (bRaisedToN printStringBase: b) = ('1' , (String new: n withAll: $0)).
			
			self assert: (bRaisedToN negated + 1 printStringBase: b) = ('-' , (String new: n withAll: (Character digitValue: b-1))).
			self assert: (bRaisedToN negated printStringBase: b) = ('-1' , (String new: n withAll: $0))]].
! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'laza 30/Mar/2004, 1:23:00 pm (UTC)'!
testRomanPrinting
	self assert: 0 printStringRoman = ''. "No symbol for zero"
	self assert: 1 printStringRoman = 'I'.
	self assert: 2 printStringRoman = 'II'.
	self assert: 3 printStringRoman = 'III'.
	self assert: 4 printStringRoman = 'IV'.
	self assert: 5 printStringRoman = 'V'.
	self assert: 6 printStringRoman = 'VI'.
	self assert: 7 printStringRoman = 'VII'.
	self assert: 8 printStringRoman = 'VIII'.
	self assert: 9 printStringRoman = 'IX'.
	self assert: 10 printStringRoman = 'X'.
	self assert: 23 printStringRoman = 'XXIII'.
	self assert: 36 printStringRoman = 'XXXVI'.
	self assert: 49 printStringRoman = 'XLIX'.
	self assert: 62 printStringRoman = 'LXII'.
	self assert: 75 printStringRoman = 'LXXV'.
	self assert: 88 printStringRoman = 'LXXXVIII'.
	self assert: 99 printStringRoman = 'XCIX'.
	self assert: 100 printStringRoman = 'C'.
	self assert: 101 printStringRoman = 'CI'.
	self assert: 196 printStringRoman = 'CXCVI'.
	self assert: 197 printStringRoman = 'CXCVII'.
	self assert: 198 printStringRoman = 'CXCVIII'.
	self assert: 293 printStringRoman = 'CCXCIII'.
	self assert: 294 printStringRoman = 'CCXCIV'.
	self assert: 295 printStringRoman = 'CCXCV'.
	self assert: 390 printStringRoman = 'CCCXC'.
	self assert: 391 printStringRoman = 'CCCXCI'.
	self assert: 392 printStringRoman = 'CCCXCII'.
	self assert: 487 printStringRoman = 'CDLXXXVII'.
	self assert: 488 printStringRoman = 'CDLXXXVIII'.
	self assert: 489 printStringRoman = 'CDLXXXIX'.
	self assert: 584 printStringRoman = 'DLXXXIV'.
	self assert: 585 printStringRoman = 'DLXXXV'.
	self assert: 586 printStringRoman = 'DLXXXVI'.
	self assert: 681 printStringRoman = 'DCLXXXI'.
	self assert: 682 printStringRoman = 'DCLXXXII'.
	self assert: 683 printStringRoman = 'DCLXXXIII'.
	self assert: 778 printStringRoman = 'DCCLXXVIII'.
	self assert: 779 printStringRoman = 'DCCLXXIX'.
	self assert: 780 printStringRoman = 'DCCLXXX'.
	self assert: 875 printStringRoman = 'DCCCLXXV'.
	self assert: 876 printStringRoman = 'DCCCLXXVI'.
	self assert: 877 printStringRoman = 'DCCCLXXVII'.
	self assert: 972 printStringRoman = 'CMLXXII'.
	self assert: 973 printStringRoman = 'CMLXXIII'.
	self assert: 974 printStringRoman = 'CMLXXIV'.
	self assert: 1069 printStringRoman = 'MLXIX'.
	self assert: 1070 printStringRoman = 'MLXX'.
	self assert: 1071 printStringRoman = 'MLXXI'.
	self assert: 1166 printStringRoman = 'MCLXVI'.
	self assert: 1167 printStringRoman = 'MCLXVII'.
	self assert: 1168 printStringRoman = 'MCLXVIII'.
	self assert: 1263 printStringRoman = 'MCCLXIII'.
	self assert: 1264 printStringRoman = 'MCCLXIV'.
	self assert: 1265 printStringRoman = 'MCCLXV'.
	self assert: 1360 printStringRoman = 'MCCCLX'.
	self assert: 1361 printStringRoman = 'MCCCLXI'.
	self assert: 1362 printStringRoman = 'MCCCLXII'.
	self assert: 1457 printStringRoman = 'MCDLVII'.
	self assert: 1458 printStringRoman = 'MCDLVIII'.
	self assert: 1459 printStringRoman = 'MCDLIX'.
	self assert: 1554 printStringRoman = 'MDLIV'.
	self assert: 1555 printStringRoman = 'MDLV'.
	self assert: 1556 printStringRoman = 'MDLVI'.
	self assert: 1651 printStringRoman = 'MDCLI'.
	self assert: 1652 printStringRoman = 'MDCLII'.
	self assert: 1653 printStringRoman = 'MDCLIII'.
	self assert: 1748 printStringRoman = 'MDCCXLVIII'.
	self assert: 1749 printStringRoman = 'MDCCXLIX'.
	self assert: 1750 printStringRoman = 'MDCCL'.
	self assert: 1845 printStringRoman = 'MDCCCXLV'.
	self assert: 1846 printStringRoman = 'MDCCCXLVI'.
	self assert: 1847 printStringRoman = 'MDCCCXLVII'.
	self assert: 1942 printStringRoman = 'MCMXLII'.
	self assert: 1943 printStringRoman = 'MCMXLIII'.
	self assert: 1944 printStringRoman = 'MCMXLIV'.
	self assert: 2004 printStringRoman = 'MMIV'.

	self assert: -1 printStringRoman = '-I'.
	self assert: -2 printStringRoman = '-II'.
	self assert: -3 printStringRoman = '-III'.
	self assert: -4 printStringRoman = '-IV'.
	self assert: -5 printStringRoman = '-V'.
	self assert: -6 printStringRoman = '-VI'.
	self assert: -7 printStringRoman = '-VII'.
	self assert: -8 printStringRoman = '-VIII'.
	self assert: -9 printStringRoman = '-IX'.
	self assert: -10 printStringRoman = '-X'.
! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:57:00 pm (UTC)'!
testModLnLn

	self assert: (-42391158275216203514294433200 mod: -14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModLnLp

	self assert: (-42391158275216203514294433200 mod: 14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:05:00 pm (UTC)'!
testModLnSn

	self assert: (-42391158275216203514294433201 mod: -3) = 0.
	self assert: (-42391158275216203514294433202 mod: -3) = 2.
	self assert: (-42391158275216203514294433203 mod: -3) = 1.
	self assert: (-42391158275216203514294433204 mod: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModLnSp

	self assert: (-42391158275216203514294433201 mod: 3) = 0.
	self assert: (-42391158275216203514294433202 mod: 3) = 2.
	self assert: (-42391158275216203514294433203 mod: 3) = 1.
	self assert: (-42391158275216203514294433204 mod: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:05:00 pm (UTC)'!
testModLpLn

	self assert: (42391158275216203514294433200 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModLpLp

	self assert: (42391158275216203514294433200 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:04:00 pm (UTC)'!
testModLpSn

	self assert: (42391158275216203514294433201 mod: -3) = 0.
	self assert: (42391158275216203514294433202 mod: -3) = 1.
	self assert: (42391158275216203514294433203 mod: -3) = 2.
	self assert: (42391158275216203514294433204 mod: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModLpSp

	self assert: (42391158275216203514294433201 mod: 3) = 0.
	self assert: (42391158275216203514294433202 mod: 3) = 1.
	self assert: (42391158275216203514294433203 mod: 3) = 2.
	self assert: (42391158275216203514294433204 mod: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:04:00 pm (UTC)'!
testModSnLn

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (-1 mod: -42391158275216203514294433201) = 42391158275216203514294433200.
	self assert: (-14348907 mod: -42391158275216203514294433201) = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModSnLp

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (-1 mod: 42391158275216203514294433201) =  42391158275216203514294433200.
	self assert: (-14348907 mod: 42391158275216203514294433201) = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:00:00 pm (UTC)'!
testModSnSn

	self assert: (0 mod: -3) = 0.
	self assert: (-1 mod: -3) = 2.
	self assert: (-9 mod: -3) = 0.
	self assert: (-10 mod: -3) = 2.
	self assert: (-11 mod: -3) = 1.
	self assert: (-12 mod: -3) = 0.
	self assert: (-13 mod: -3) = 2! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:59:00 pm (UTC)'!
testModSnSp

	self assert: (0 mod: 3) = 0.
	self assert: (-1 mod: 3) = 2.
	self assert: (-9 mod: 3) = 0.
	self assert: (-10 mod: 3) = 2.
	self assert: (-11 mod: 3) = 1.
	self assert: (-12 mod: 3) = 0.
	self assert: (-13 mod: 3) = 2! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 7:05:00 pm (UTC)'!
testModSpLn

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (1 mod: -42391158275216203514294433201) = 1.
	self assert: (14348907 mod: -42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModSpLp

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (1 mod: 42391158275216203514294433201) = 1.
	self assert: (14348907 mod: 42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:58:00 pm (UTC)'!
testModSpSn

	self assert: (0 mod: -3) = 0.
	self assert: (1 mod: -3) = 1.
	self assert: (9 mod: -3) = 0.
	self assert: (10 mod: -3) = 1.
	self assert: (11 mod: -3) = 2.
	self assert: (12 mod: -3) = 0.
	self assert: (13 mod: -3) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testModSpSp

	self assert: (0 mod: 3) = 0.
	self assert: (1 mod: 3) = 1.
	self assert: (9 mod: 3) = 0.
	self assert: (10 mod: 3) = 1.
	self assert: (11 mod: 3) = 2.
	self assert: (12 mod: 3) = 0.
	self assert: (13 mod: 3) = 1! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 15/Oct/2016, 2:58:02 pm (UTC)'!
testNthRootExactnessForHugeValue
	"
	self new testNthRootExactnessForHugeValue
	"
        self assert: ((10 raisedTo: 600) nthRoot: 300) = 100.
        self assert: ((10 raisedTo: 600) nthRoot: 300) class == SmallInteger.
        self assert: ((10 raisedTo: 600) + 1 nthRoot: 300) = 100.0.
        self assert: (((10 raisedTo: 600) + 1 nthRoot: 300) isKindOf: Float)! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 15/Oct/2016, 2:57:28 pm (UTC)'!
testNthRootImmuneToDoubleRounding
"
self new testNthRootImmuneToDoubleRounding
"
          "Use a specially crafted number for causing double rounding.
         Solution is 10...01.1 - verySmallQuantity.
         Where verySmallQuantity is approximately 1/53/(1<<53).
         If the verySmallQuantity is not taken into account, then solution is rounded to 10....010"
         | exponent crafted root highPrecisionRoot |
         exponent := 4.
         crafted := (1 << Float precision + 3 raisedTo: exponent) - 1.
         root := crafted nthRoot: exponent.
         highPrecisionRoot := (crafted << (exponent squared * Float precision * 4) nthRootRounded: exponent) / (1 << (exponent * Float precision * 4)).
         self assert: (root asFraction - highPrecisionRoot) abs < (root predecessor asFraction - highPrecisionRoot) abs.
         
         "Same with the other sign.
         Solution is 10...00.1 + verySmallQuantity."
         crafted := (1 << Float precision + 1 raisedTo: exponent) + 1.
         root := crafted nthRoot: exponent.
         highPrecisionRoot := (crafted << (exponent squared * Float precision * 4) nthRootRounded: exponent) / (1 << (exponent * Float precision * 4)).
         self assert: (root asFraction - highPrecisionRoot) abs < (root successor asFraction - highPrecisionRoot) abs.! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 24/May/2024, 7:14:56 pm (UTC)'!
testSinCosForHugeValues1
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e22.
	equivalentFloatInTwoPiRange := Float twoPi - (10202/10000).
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e22 + i.
		equivalentFloatInTwoPiRange := Float twoPi - (10202/10000) + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].
! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 24/May/2024, 7:14:48 pm (UTC)'!
testSinCosForHugeValues2
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e500.
	equivalentFloatInTwoPiRange := Float twoPi + (282532 / 100000).
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e500 + i.
		equivalentFloatInTwoPiRange := Float twoPi + (282532 / 100000) + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].
! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:36:00 pm (UTC)'!
testQuoLnLn

	self assert: (-42391158275216203514294433200 quo: -14130386091738734504764811067) = 2.
	self assert: (-42391158275216203514294433201 quo: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 quo: -14130386091738734504764811067) = 3.
	self assert: (-8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = 109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:51:00 pm (UTC)'!
testQuoLnLp

	self assert: (-42391158275216203514294433200 quo: 14130386091738734504764811067) = -2.
	self assert: (-42391158275216203514294433201 quo: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 quo: 14130386091738734504764811067) = -3.
	self assert: (-8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = -109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoLnSn

	self assert: (-42391158275216203514294433201 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: -3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:51:00 pm (UTC)'!
testQuoLnSp

	self assert: (-42391158275216203514294433201 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: 3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:52:00 pm (UTC)'!
testQuoLpLn

	self assert: (42391158275216203514294433200 quo: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 quo: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 quo: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoLpLp

	self assert: (42391158275216203514294433200 quo: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 quo: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 quo: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:50:00 pm (UTC)'!
testQuoLpSn

	self assert: (42391158275216203514294433201 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: -3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoLpSp

	self assert: (42391158275216203514294433201 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: 3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoSnLn

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (-1 quo: -42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:50:00 pm (UTC)'!
testQuoSnLp

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (-1 quo: 42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoSnSn

	self assert: (0 quo: -3) = 0.
	self assert: (-1 quo: -3) = 0.
	self assert: (-9 quo: -3) = 3.
	self assert: (-10 quo: -3) = 3.
	self assert: (-11 quo: -3) = 3.
	self assert: (-12 quo: -3) = 4.
	self assert: (-13 quo: -3) = 4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:51:00 pm (UTC)'!
testQuoSnSp

	self assert: (0 quo: 3) = 0.
	self assert: (-1 quo: 3) = 0.
	self assert: (-9 quo: 3) = -3.
	self assert: (-10 quo: 3) = -3.
	self assert: (-11 quo: 3) = -3.
	self assert: (-12 quo: 3) = -4.
	self assert: (-13 quo: 3) = -4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:52:00 pm (UTC)'!
testQuoSpLn

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (1 quo: -42391158275216203514294433201) = 0.
	self assert: (14348907 quo: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoSpLp

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (1 quo: 42391158275216203514294433201) = 0.
	self assert: (14348907 quo: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:50:00 pm (UTC)'!
testQuoSpSn

	self assert: (0 quo: -3) = 0.
	self assert: (1 quo: -3) = 0.
	self assert: (9 quo: -3) = -3.
	self assert: (10 quo: -3) = -3.
	self assert: (11 quo: -3) = -3.
	self assert: (12 quo: -3) = -4.
	self assert: (13 quo: -3) = -4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 25/Jan/2014, 6:33:00 pm (UTC)'!
testQuoSpSp

	self assert: (0 quo: 3) = 0.
	self assert: (1 quo: 3) = 0.
	self assert: (9 quo: 3) = 3.
	self assert: (10 quo: 3) = 3.
	self assert: (11 quo: 3) = 3.
	self assert: (12 quo: 3) = 4.
	self assert: (13 quo: 3) = 4! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemLnLn

	self assert: (-42391158275216203514294433200 rem: -14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: -14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:47:00 pm (UTC)'!
testRemLnLp

	self assert: (-42391158275216203514294433200 rem: 14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: 14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemLnSn

	self assert: (-42391158275216203514294433201 rem: -3) = 0.
	self assert: (-42391158275216203514294433202 rem: -3) = -1.
	self assert: (-42391158275216203514294433203 rem: -3) = -2.
	self assert: (-42391158275216203514294433204 rem: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:49:00 pm (UTC)'!
testRemLnSp

	self assert: (-42391158275216203514294433201 rem: 3) = 0.
	self assert: (-42391158275216203514294433202 rem: 3) = -1.
	self assert: (-42391158275216203514294433203 rem: 3) = -2.
	self assert: (-42391158275216203514294433204 rem: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:48:00 pm (UTC)'!
testRemLpLn

	self assert: (42391158275216203514294433200 rem: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemLpLp

	self assert: (42391158275216203514294433200 rem: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:49:00 pm (UTC)'!
testRemLpSn

	self assert: (42391158275216203514294433201 rem: -3) = 0.
	self assert: (42391158275216203514294433202 rem: -3) = 1.
	self assert: (42391158275216203514294433203 rem: -3) = 2.
	self assert: (42391158275216203514294433204 rem: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemLpSp

	self assert: (42391158275216203514294433201 rem: 3) = 0.
	self assert: (42391158275216203514294433202 rem: 3) = 1.
	self assert: (42391158275216203514294433203 rem: 3) = 2.
	self assert: (42391158275216203514294433204 rem: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemSnLn

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (-1 rem: -42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: -42391158275216203514294433201) = -14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:47:00 pm (UTC)'!
testRemSnLp

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (-1 rem: 42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: 42391158275216203514294433201) = -14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemSnSn

	self assert: (0 rem: -3) = 0.
	self assert: (-1 rem: -3) = -1.
	self assert: (-9 rem: -3) = 0.
	self assert: (-10 rem: -3) = -1.
	self assert: (-11 rem: -3) = -2.
	self assert: (-12 rem: -3) = 0.
	self assert: (-13 rem: -3) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:37:00 pm (UTC)'!
testRemSnSp

	self assert: (0 rem: 3) = 0.
	self assert: (-1 rem: 3) = -1.
	self assert: (-9 rem: 3) = 0.
	self assert: (-10 rem: 3) = -1.
	self assert: (-11 rem: 3) = -2.
	self assert: (-12 rem: 3) = 0.
	self assert: (-13 rem: 3) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:38:00 pm (UTC)'!
testRemSpLn

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (1 rem: -42391158275216203514294433201) = 1.
	self assert: (14348907 rem: -42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemSpLp

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (1 rem: 42391158275216203514294433201) = 1.
	self assert: (14348907 rem: 42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:48:00 pm (UTC)'!
testRemSpSn

	self assert: (0 rem: -3) = 0.
	self assert: (1 rem: -3) = 1.
	self assert: (9 rem: -3) = 0.
	self assert: (10 rem: -3) = 1.
	self assert: (11 rem: -3) = 2.
	self assert: (12 rem: -3) = 0.
	self assert: (13 rem: -3) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 25/Jan/2014, 6:35:00 pm (UTC)'!
testRemSpSp

	self assert: (0 rem: 3) = 0.
	self assert: (1 rem: 3) = 1.
	self assert: (9 rem: 3) = 0.
	self assert: (10 rem: 3) = 1.
	self assert: (11 rem: 3) = 2.
	self assert: (12 rem: 3) = 0.
	self assert: (13 rem: 3) = 1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:56:00 pm (UTC)'!
testSlashSlashLnLn

	self assert: -42391158275216203514294433200 // -14130386091738734504764811067 = 2.
	self assert: -42391158275216203514294433201 // -14130386091738734504764811067 = 3.
	self assert: -42391158275216203514294433202 // -14130386091738734504764811067 = 3.
	self assert: -8727963568087712425891397479476727340041448 // -79766443076872509863361 = 109418989131512359208.
	self assert: -8727963568087712425891397479476727340041449 // -79766443076872509863361 = 109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // -79766443076872509863361 = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:47:00 pm (UTC)'!
testSlashSlashLnLp

	self assert: -42391158275216203514294433200 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433201 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433202 // 14130386091738734504764811067 = -4.
	self assert: -8727963568087712425891397479476727340041448 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041449 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // 79766443076872509863361 = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:54:00 pm (UTC)'!
testSlashSlashLnSn

	self assert: -42391158275216203514294433201 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433203 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433204 // -3 = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:45:00 pm (UTC)'!
testSlashSlashLnSp

	self assert: -42391158275216203514294433201 // 3 = -14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433203 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433204 // 3 = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:54:00 pm (UTC)'!
testSlashSlashLpLn

	self assert: 42391158275216203514294433200 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433201 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433202 // -14130386091738734504764811067 = -4.
	self assert: 8727963568087712425891397479476727340041448 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041449 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // -79766443076872509863361 = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:52:00 pm (UTC)'!
testSlashSlashLpSn

	self assert: 42391158275216203514294433201 // -3 = -14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433203 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433204 // -3 = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:52:00 pm (UTC)'!
testSlashSlashSnLn

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: -1 // -42391158275216203514294433201 = 0.
	self assert: -14348907 // -42391158275216203514294433201 = 0! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:44:00 pm (UTC)'!
testSlashSlashSnLp

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: -1 // 42391158275216203514294433201 = -1.
	self assert: -14348907 // 42391158275216203514294433201 = -1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:51:00 pm (UTC)'!
testSlashSlashSnSn

	self assert: 0 // -3 = 0.
	self assert: -1 // -3 = 0.
	self assert: -9 // -3 = 3.
	self assert: -10 // -3 = 3.
	self assert: -11 // -3 = 3.
	self assert: -12 // -3 = 4.
	self assert: -13 // -3 = 4! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:42:00 pm (UTC)'!
testSlashSlashSnSp

	self assert: 0 // 3 = 0.
	self assert: -1 // 3 = -1.
	self assert: -9 // 3 = -3.
	self assert: -10 // 3 = -4.
	self assert: -11 // 3 = -4.
	self assert: -12 // 3 = -4.
	self assert: -13 // 3 = -5! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:51:00 pm (UTC)'!
testSlashSlashSpLn

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: 1 // -42391158275216203514294433201 = -1.
	self assert: 14348907 // -42391158275216203514294433201 = -1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:44:00 pm (UTC)'!
testSlashSlashSpLp

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: 1 // 42391158275216203514294433201 = 0.
	self assert: 14348907 // 42391158275216203514294433201 = 0! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:50:00 pm (UTC)'!
testSlashSlashSpSn

	self assert: 0 // -3 = 0.
	self assert: 1 // -3 = -1.
	self assert: 9 // -3 = -3.
	self assert: 10 // -3 = -4.
	self assert: 11 // -3 = -4.
	self assert: 12 // -3 = -4.
	self assert: 13 // -3 = -5! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 25/Jan/2014, 5:42:00 pm (UTC)'!
testSlashSlashSpSp

	self assert: 0 // 3 = 0.
	self assert: 1 // 3 = 0.
	self assert: 9 // 3 = 3.
	self assert: 10 // 3 = 3.
	self assert: 11 // 3 = 3.
	self assert: 12 // 3 = 4.
	self assert: 13 // 3 = 4! !

!IntegerTest methodsFor: 'tests - division' stamp: 'sqr 25/Jan/2014, 5:38:00 pm (UTC)'!
testSlashSlashLpLp

	self assert: 42391158275216203514294433200 // 14130386091738734504764811067 = 2.
	self assert: 42391158275216203514294433201 // 14130386091738734504764811067 = 3.
	self assert: 42391158275216203514294433202 // 14130386091738734504764811067 = 3.
	self assert: 8727963568087712425891397479476727340041448 // 79766443076872509863361 = 109418989131512359208.
	self assert: 8727963568087712425891397479476727340041449 // 79766443076872509863361 = 109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // 79766443076872509863361 = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division' stamp: 'sqr 25/Jan/2014, 5:33:00 pm (UTC)'!
testSlashSlashLpSp

	self assert: 42391158275216203514294433201 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433203 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433204 // 3 = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'as yet unclassified' stamp: 'jmv 10/Sep/2018, 2:28:17 pm (UTC)'!
test01readFromWithEmptyStringRaisesAnError
	|input|
	
	input := ReadStream on: ''.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'! !

!IntegerTest methodsFor: 'as yet unclassified' stamp: 'jmv 10/Sep/2018, 2:28:20 pm (UTC)'!
test02readFromWithInvalidStringRaisesAnError
	| input |
	
	input := ReadStream on: 'hello'.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 27/Jun/2019, 2:23:39 pm (UTC)'!
testGCDAndLCM
	| a b c |
	a _ (-20 to: 20) atRandom.
	b _ (-20 to: 20) atRandom.
	c _ (-20 to: 20) atRandom.
	self assert: (a gcd: b) * (a lcm: b) = (a*b) abs.
	self assert: (a gcd: (b lcm: c)) = ((a gcd: b) lcm: (a gcd: c)).
	self assert: (a lcm: (b gcd: c)) = ((a lcm: b) gcd: (a lcm: c))! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 27/Jun/2019, 2:25:17 pm (UTC)'!
testGCDExample
	self assert: (120 gcd: 70) = 10! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'jmv 27/Jan/2025, 2:04:37 pm (UTC)'!
testGCDProperties
	| a b c |
	[
		a := (-20 to: 20) atRandom.
		b := (-20 to: 20) atRandom.
		c := (-20 to: 20) atRandom.
		a = 0 and: [ b = 0 ]] whileTrue. "yes, it can happen"
	self assert: a \\ (a gcd: b) = 0. "divides a"
	self assert: b \\ (a gcd: b) = 0. "divides b"
	self assert: (a gcd: b) = (a gcd: b) abs. "it is normalized to be >= 0"
	self assert: (a gcd: b) = (b gcd: a). "commutative"
	self assert: (a gcd: (b gcd: c)) = ((a gcd: b) gcd: c). "associative"
	self assert: ((a gcd: 0) = a abs or: [a = 0]).
	self assert: (0 gcd: 0) = 0.
	self assert: (a + (b*c) gcd: b) = (a gcd: b).
	self assert: (a*c gcd: b*c) = ((a gcd: b) * c abs)! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 27/Jun/2019, 2:50:25 pm (UTC)'!
testLCMExample
	self assert: (120 lcm: 70) = 840! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 27/Jun/2019, 2:48:22 pm (UTC)'!
testLCMProperties
	| a b c |
	a _ (-10 to: 10) atRandom.
	b _ (-10 to: 10) atRandom.
	c _ (-10 to: 10) atRandom.
	self assert: (a = 0 or: [(a lcm: b) \\ a = 0]). "a divides it"
	self assert: (b = 0 or: [(a lcm: b) \\ b = 0]). "b divides it"
	"is is the smallest positive integer divisible by both a and b:"
	self deny: ((a ~= 0 and: [b ~= 0]) and: [(1 to: (a lcm: b) abs - 1) anySatisfy: [:any| any \\ a = 0 and: [any \\ b = 0]]]).
	self assert: (a lcm: b) = (a lcm: b) abs. "it is normalized to be >= 0"
	self assert: (a lcm: b) = (b lcm: a). "commutative"
	self assert: (a lcm: (b lcm: c)) = ((a lcm: b) lcm: c). "associative"
	self assert: (a lcm: 0) = 0.
	self assert: (0 lcm: a) = 0.
	self assert: (0 lcm: 0) = 0! !

!IntegerTest methodsFor: 'tests - arithmetic' stamp: 'jmv 24/May/2024, 4:35:17 pm (UTC)'!
testReciprocalModulo
	1 to: 512 do: [:a |
		a + 1 to: 512 do: [:b |
			| c |
			(a gcd: b) = 1
				ifTrue:
					[c := a reciprocalModulo: b.
					self assert: (a * c) \\ b = 1]
				ifFalse: [self assert: ( a reciprocalModulo: b ) isNil]]].! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'ul 15/Nov/2010, 3:51:00 pm (UTC)'!
testDenormalizedPrintString
	"Check that an un-normalized instance behaves reasonably."

	| i i0 |
	i := LargeNegativeInteger new: 4.
	i basicAt: 2 put: 255.
	self assert: i size = 4.
	self assert: i printString = '-65280'.	"-256*255"
	self assert: i normalize = -65280.
	self assert: (i normalize isMemberOf: SmallInteger).
	
	i0 := LargeNegativeInteger new: 0.
	self assert: i0 size = 0.
	self assert: i0 printString = '-0'.
	self assert: i0 normalize = 0.
	self assert: (i0 normalize isMemberOf: SmallInteger)! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:18:36 pm (UTC)'!
testDigitAt

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lni digitAt: i.
		self assert: digit equals: i]
! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:19:22 pm (UTC)'!
testDigitAtPut

	| lni |
	lni := LargeNegativeInteger new: 20.
	1 to: 20 do: [:i | lni digitAt: i put: i].
	self assert: lni equals: -114605103402541699037609980192546360895434064385
! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:19:42 pm (UTC)'!
testDigitLength

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	self assert: lni digitLength equals: 20! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'ul 15/Nov/2010, 3:52:00 pm (UTC)'!
testEmptyTemplate
	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargeNegativeInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '-0'.
	self assert: i normalize = 0.
	self assert: (i normalize isMemberOf: SmallInteger)! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'nice 1/Sep/2012, 3:00:00 am (UTC)'!
testMinimumNegativeIntegerArithmetic
	"We are speaking of minimum integer in underlying hardware here.
	In 2-complement, abs(INT_MIN) = (INT-MAX+1) and thus overflows hardware register.
	Since some old VM forgot this edge case they may fail and it's better to be aware of it.
	http://code.google.com/p/cog/issues/detail?id=92
	http://bugs.squeak.org/view.php?id=7705
	We only test the cases of 32 and 64 bit signed integers."

	#(32 64) do: [:nBits |
		| largePositiveInt largeNegativeInt |
		largePositiveInt := (1 << (nBits - 1)).
		largeNegativeInt := largePositiveInt negated.
		self assert: (largeNegativeInt >> 3) equals: (largeNegativeInt bitInvert >> 3) bitInvert.
		self assert: (largeNegativeInt + 1) equals: (largePositiveInt - 1) negated.
		self assert: (largeNegativeInt - -1) equals: (largePositiveInt - 1) negated.
		self assert: (largeNegativeInt // -1) equals: largePositiveInt.
		self assert: (largeNegativeInt \\ -1) equals: 0.
		self assert: (largeNegativeInt rem: -1) equals: 0.
		self assert: (largeNegativeInt quo: -1) equals: largePositiveInt.
		self assert: (largeNegativeInt * -1) equals: largePositiveInt.
		self assert: (largeNegativeInt / -1) equals: largePositiveInt]! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:21:05 pm (UTC)'!
testReplaceFromToWithStartingAt

	| lni20 lni7 |
	lni20 := LargeNegativeInteger new: 20.
	1 to: 20 do: [:i | lni20 digitAt: i put: i].
	lni7 := LargeNegativeInteger new: 7.
	1 to: 7 do: [:i | lni7 digitAt: i put: 11 - i].
	lni20 replaceFrom: 6 to: 10 with: lni7 startingAt: 2.
	"unmodified digits"
	(1 to: 5) , (11 to: 20) do: [:e | | digit |
		digit := lni20 digitAt: e.
		self assert: digit equals: e].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lni20 digitAt: e.
		replacementDigit := lni7 digitAt: e - 4.
		self assert: digit equals: replacementDigit]
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 6/Jun/2023, 3:34:02 pm (UTC)'!
test_raisedTo_modulo
	"The bug was caused because of calling unnormalized #\\\.
	The fix is to use #\\ instead."

	self assert: (5445648384 raisedTo: 3 modulo: 1 << 34) = 0.
	self assert: (5445648384 raisedTo: 4 modulo: 1 << 34) = 0.! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 1/Sep/2010, 5:47:00 pm (UTC)'!
testBitShift

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) = i]].! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'ul 15/Nov/2010, 3:52:00 pm (UTC)'!
testDenormalizedPrintString
	"Check that an un-normalized instance behaves reasonably."

	| i i0 |
	i := LargePositiveInteger new: 4.
	i basicAt: 2 put: 255.
	self assert: i size = 4.
	self assert: i printString = '65280'.	"256*255"
	self assert: i normalize = 65280.
	self assert: (i normalize isMemberOf: SmallInteger).
	
	i0 := LargePositiveInteger new: 0.
	self assert: i0 size = 0.
	self assert: i0 printString = '0'.
	self assert: i0 normalize = 0.
	self assert: (i0 normalize isMemberOf: SmallInteger)! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:21:17 pm (UTC)'!
testDigitAt

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lpi digitAt: i.
		self assert: digit equals: i]
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:21:34 pm (UTC)'!
testDigitAtPut

	| lpi |
	lpi := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi digitAt: i put: i].
	self assert: lpi equals: 114605103402541699037609980192546360895434064385
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:21:48 pm (UTC)'!
testDigitLength

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	self assert: lpi digitLength equals: 20
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'ul 15/Nov/2010, 3:52:00 pm (UTC)'!
testEmptyTemplate

	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '0'.
	self assert: i normalize = 0.
	self assert: (i normalize isMemberOf: SmallInteger)! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'nice 21/Mar/2014, 10:38:00 pm (UTC)'!
testLargeSqrtFloor
	"This test fails if a careless implementation naivly factors out the power of two (remove the trailing zeroes up to lowBit).
	This was the case in a previous Squeak 4.x implementation."

	| large root |
	large := (SmallInteger maxVal << 100 + 1) << 100.
	root := large sqrtFloor.
	self assert: root squared <= large.
	self assert: (root+1) squared > large.! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'sd 5/Jun/2005, 12:52:00 pm (UTC)'!
testMultDicAddSub
	"self run: #testMultDicAddSub"

	| n f f1 |	
	n := 100.
	f := 100 factorial.
	f1 := f*(n+1).
	n timesRepeat: [f1 := f1 - f].
	self assert: (f1 = f). 

	n timesRepeat: [f1 := f1 + f].
	self assert: (f1 // f = (n+1)). 
	self assert: (f1 negated = (Number readFrom: '-' , f1 printString)).! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 1/Sep/2010, 5:48:00 pm (UTC)'!
testNormalize
	"self run: #testNormalize"
	"Check normalization and conversion to/from SmallInts"

	self assert: ((SmallInteger maxVal + 1 - 1) = SmallInteger maxVal).
	self assert: (SmallInteger maxVal + 3 - 6) = (SmallInteger maxVal-3).
	self should: ((SmallInteger minVal - 1 + 1) = SmallInteger minVal).
	self assert: (SmallInteger minVal - 3 + 6) = (SmallInteger minVal+3).! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:22:06 pm (UTC)'!
testReplaceFromToWithStartingAt

	| lpi20 lpi7 |
	lpi20 := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi20 digitAt: i put: i].
	lpi7 := LargePositiveInteger new: 7.
	1 to: 7 do: [:i | lpi7 digitAt: i put: 11 - i].
	lpi20 replaceFrom: 6 to: 10 with: lpi7 startingAt: 2.
	"unmodified digits"
	(1 to: 5) , (11 to: 20) do: [:e | | digit |
		digit := lpi20 digitAt: e.
		self assert: digit equals: e].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lpi20 digitAt: e.
		replacementDigit := lpi7 digitAt: e - 4.
		self assert: digit equals: replacementDigit]
! !

!LargePositiveIntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 13/May/2019, 5:56:00 pm (UTC)'!
testSqrt
	self assert: (SmallInteger maxVal + 1) sqrt equals: (SmallInteger maxVal + 1) asFloat sqrt.! !

!NumberTest methodsFor: 'tests' stamp: 'nice 5/Mar/2021, 1:14:00 am (UTC)'!
testExactLog2
	-10 to: 10 do: [:i | self assert: i equals: (2 raisedToInteger: i) log2].
	Float emin - Float precision + 1 to: Float emax do: [:i | self assert: i equals: (1.0 timesTwoPower: i) log2].! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 27/May/2009, 1:57:00 am (UTC)'!
testFractionPart

	self 
		assert: 2 fractionPart = 0;
		assert: (1/2) fractionPart = (1/2);
		assert: (4/3) fractionPart = (1/3);
		assert: 2.0 fractionPart = 0.0;
		assert: 0.5 fractionPart = 0.5;
		assert: 2.5 fractionPart = 0.5
! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 21/Nov/2018, 9:38:59 pm (UTC)'!
testHash1
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallInteger |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 1;
			digitAt: 2 put: 2;
			digitAt: 3 put: 3;
			digitAt: 4 put: 4;
			yourself.
	smallInteger := largeInteger normalize.
	float := smallInteger asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallInteger hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 21/Nov/2018, 9:39:36 pm (UTC)'!
testHash2
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallIntIn64ButLargeIntIn32Bits |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 0;
			digitAt: 2 put: 0;
			digitAt: 3 put: 0;
			digitAt: 4 put: 64;
			yourself.
	smallIntIn64ButLargeIntIn32Bits := largeInteger normalize.
	float := smallIntIn64ButLargeIntIn32Bits asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallIntIn64ButLargeIntIn32Bits hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 21/Nov/2018, 9:39:51 pm (UTC)'!
testHash3
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallIntIn64ButLargeIntIn32Bits |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 1;
			digitAt: 2 put: 2;
			digitAt: 3 put: 3;
			digitAt: 4 put: 80;
			yourself.
	smallIntIn64ButLargeIntIn32Bits := largeInteger normalize.
	float := smallIntIn64ButLargeIntIn32Bits asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallIntIn64ButLargeIntIn32Bits hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 27/May/2009, 1:55:00 am (UTC)'!
testIntegerPart

	self 
		assert: 2 integerPart = 2;
		assert: (1/2) integerPart = 0;
		assert: (4/3) integerPart = 1;
		assert: 2.0 integerPart = 2.0;
		assert: 0.5 integerPart = 0.0;
		assert: 2.5 integerPart = 2.0
! !

!NumberTest methodsFor: 'tests' stamp: 'nice 5/Mar/2021, 1:13:00 am (UTC)'!
testLog2doesNotOverflow
	"Note: though this is not a strict identity, we can use strict Float equality here"
	self assert: 3000.0 equals: ((1 bitShift: 3000) - 1) log2.
	self assert: 1500.0 equals: (((1 bitShift: 3000) - 1) / (1 bitShift: 1500)) log2.! !

!NumberTest methodsFor: 'tests' stamp: 'nice 5/Mar/2021, 1:13:00 am (UTC)'!
testLog2doesNotUnderflow
	"Note: though this is not a strict identity, we can use strict Float equality here"
	self assert: -2000.0 equals: ((1 bitShift: 2000) - 1) reciprocal log2! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 24/May/2009, 12:49:00 am (UTC)'!
testOne

	self 
		assert: Integer one = 1;
		assert: Float one = 1.0;
		assert: Fraction one = 1! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 10/Jan/2014, 1:23:00 am (UTC)'!
testPrintShowingDecimalPlaces

	self assert: (String streamContents: [ :s | 111.2 printOn: s fractionDigits: 2]) = '111.20'.
	self assert: (String streamContents: [ :s | 111.2 printOn: s fractionDigits: 0]) = '111'.
	self assert: (String streamContents: [ :s | 111 printOn: s fractionDigits: 0]) = '111'.
	self assert: (String streamContents: [ :s | 111111111111111 printOn: s fractionDigits: 2]) = '111111111111111.00'.
	self assert: (String streamContents: [ :s | 10 printOn: s fractionDigits: 20]) ='10.00000000000000000000'.
	self assert: (String streamContents: [ :s | 0.98 printOn: s fractionDigits: 2]) = '0.98'.
	self assert: (String streamContents: [ :s | -0.98 printOn: s fractionDigits: 2]) = '-0.98'.
	self assert: (String streamContents: [ :s | 2.567 printOn: s fractionDigits: 2]) = '2.57'.
	self assert: (String streamContents: [ :s | -2.567 printOn: s fractionDigits: 2]) = '-2.57'! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 21/Dec/2018, 4:00:50 pm (UTC)'!
testPrintShowingDecimalPlaces2
	"This tests problems related to Float>>rounded and Float>>roundTo::
	- Float>>#rounded is inexact
	- Float>>#roundTo: might overflow"

	"This number is represented exactly asFloat, it should print exactly"
	self assert: 5000000000000001.0 asTrueFraction = 5000000000000001.
	self assert: 5000000000000001 highBit = 53.
	self assert: (5000000000000001.0 printStringFractionDigits: 0) = '5000000000000001'.
	
	"50000000000001.25 asTrueFraction = (200000000000005/4).
	200000000000005 highBit = 48, 4 isPowerOfTwo,
	So this number is also represented exactly as Float, it should print exactly.
	Beware: (50000000000001.25 / 0.01) rounded exhibit the same problem as above."
	self assert: (String streamContents: [ :s | 50000000000001.25 printOn: s fractionDigits: 2]) = '50000000000001.25'.

	"Since 4 isPowerOfTwo, this number is also represented exactly as Float, it should print exactly"
	self assert: 50000000000001.25 asTrueFraction = (200000000000005/4).
	self assert: 200000000000005 highBit = 48.
	self assert: (50000000000001.25 printStringFractionDigits: 2) = '50000000000001.25'.
	
	"This number is close to fmax"
	self shouldnt: [String streamContents: [ :s | 1.0e306 printOn: s fractionDigits: 3]] raise: Error.! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 10/Jan/2014, 1:22:00 am (UTC)'!
testPrintShowingDecimalPlaces3
	"This problem were reported at http://bugs.squeak.org/view.php?id=7028
	unfortunate inversion of left / right padding"
	self assert: (String streamContents: [ :s | 1.009 printOn: s fractionDigits: 3]) = '1.009'.
	self assert: (String streamContents: [ :s | 35.900 printOn: s fractionDigits: 3]) = '35.900'.
	self assert: (String streamContents: [ :s | -0.097 printOn: s fractionDigits: 3]) = '-0.097'.! !

!NumberTest methodsFor: 'tests' stamp: 'nice 7/Dec/2007, 1:24:00 am (UTC)'!
testRaisedTo
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 25/May/2009, 8:41:00 pm (UTC)'!
testRaisedToInteger

	self 
		assert: (2 raisedToInteger: 0) = 1;
		assert: (2 raisedToInteger: 1) = 2;
		assert: (2 raisedToInteger: 4) = 16;
		assert: (0 raisedToInteger: 0) = 1;
		assert: (0 raisedToInteger: 2) = 0;
		assert: (2 raisedToInteger: -1) = (1/2);
		assert: (2 raisedToInteger: -4) = (1/16).
	
	self 
		assert: (-3 raisedTo: 0) = 1;
		assert: (-3 raisedTo: 1) = -3;
		assert: (-3 raisedTo: 2) = 9;
		assert: (-3 raisedTo: 3) = -27;
		assert: (-3 raisedTo: -2) = (1/9);
		assert: (-3 raisedTo: -3) = (-1/27).
	
	self should: [ 0 raisedTo: -1 ] raise: ZeroDivide! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 5/Apr/2019, 9:38:36 pm (UTC)'!
testRaisedToIntegerWithFloats

	self 
		assert: (2.0 raisedToInteger: 0) = 1.0;
		assert: (2.0 raisedToInteger: 1) = 2.0;
		assert: (2.0 raisedToInteger: 4) = 16.0;
		assert: (0.0 raisedToInteger: 0) = 1.0;
		assert: (0.0 raisedToInteger: 2) = 0.0;
		assert: (2.0 raisedToInteger: -1) = 0.5;
		assert: (2.0 raisedToInteger: -4) = 0.0625;
		assert: (Float e raisedToInteger: -746) = 0.0;
		assert: (Float e raisedToInteger: -745) = 0.0 successor;
		assert: (Float e raisedToInteger: -744) = 0.0 successor successor.	
	self 
		assert: (-3.0 raisedToInteger: 0) = 1.0;
		assert: (-3.0 raisedToInteger: 1) = -3.0;
		assert: (-3.0 raisedToInteger: 2) = 9.0;
		assert: (-3.0 raisedToInteger: 3) = -27.0;
		assert: (-2.0 raisedToInteger: -2) = 0.25;
		assert: (-2.0 raisedToInteger: -3) = -0.125;
		assert: (Float e negated raisedToInteger: -746) = 0.0;
		assert: (Float e negated raisedToInteger: -745) = -0.0 predecessor;
		assert: (Float e negated raisedToInteger: -744) = 0.0 successor successor.
	
	self should: [ 0.0 raisedToInteger: -1 ] raise: ZeroDivide! !

!NumberTest methodsFor: 'tests' stamp: 'sd 5/Jun/2005, 12:56:00 pm (UTC)'!
testReadFrom
	
	self assert: 1.0e-14	= (Number readFrom: '1.0e-14').
	self assert: 2r1e26	= (Number readFrom: '2r1e26').! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 23/May/2009, 11:26:00 pm (UTC)'!
testReciprocal

	self 
		assert: 1 reciprocal = 1;
		assert: 2 reciprocal = (1/2);
		assert: -1 reciprocal = -1;
		assert: -3 reciprocal = (-1/3).
		
	self should: [ 0 reciprocal ] raise: ZeroDivide! !

!NumberTest methodsFor: 'tests' stamp: 'sqr 26/Jun/2019, 1:46:32 pm (UTC)'!
testZeroDivideHandler
	"Test for user-defined ZeroDivide behavior"

	[
		self assert: 1 / 0 = Float infinity.
		self assert: -1 / 0 = Float negativeInfinity.
		self assert: 1.0 / 0 = Float infinity.
		self assert: -1.0 / 0 = Float negativeInfinity.
		self assert: 1 / 0.0 = Float infinity.
		self assert: -1 / 0.0 = Float negativeInfinity.
		self assert: 1.0 / 0.0 = Float infinity.
		self assert: -1.0 / 0.0 = Float negativeInfinity.
	]	on: ZeroDivide
		do: [:ex | ex resume: ex receiver sign * Float infinity ]
! !

!NumberTest methodsFor: 'test' stamp: 'GC 8/Sep/2018, 6:51:43 pm (UTC)'!
test01readFromWhenTheInitilValueIsNaNTheResultIsNaN

	|initialValue nan result |
	
	nan := Float nan.
	initialValue := 'NaN'.
	result := self subject: initialValue.
	
	self assert: result is: nan! !

!NumberTest methodsFor: 'test' stamp: 'GC 8/Sep/2018, 7:07:25 pm (UTC)'!
test02readFromWhenTheInitilValueIsInfinityTheResultIsInfinity
	
	self assertThat: 'Infinity' isEqualTo: Float infinity! !

!NumberTest methodsFor: 'test' stamp: 'GC 8/Sep/2018, 7:07:50 pm (UTC)'!
test03readFromWhenTheInitilValueIsMinusInfinityTheResultIsNegativeInfinity

	self assertThat: '-Infinity' isEqualTo: Float negativeInfinity! !

!NumberTest methodsFor: 'test' stamp: 'GC 8/Sep/2018, 7:08:34 pm (UTC)'!
test04readFromWhenTheInitilValueIsANegativeNumberTheResultIsThatNumberNegated
	
	self assertThat: '-3' isEqualTo: -3! !

!NumberTest methodsFor: 'test' stamp: 'GC 8/Sep/2018, 7:09:00 pm (UTC)'!
test05readFromWhenTheInitilValueIsJustANumberTheResultIsThatNumber
	
	self assertThat: '3' isEqualTo: 3! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/Sep/2018, 8:32:38 pm (UTC)'!
test06readFromWhenTheInitialValueIncludesRadixItIsConvertedWithTheIndicatedRadixBase
	|radixBase number initualNumber |
	
	radixBase := '2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: 30! !

!NumberTest methodsFor: 'test' stamp: 'jmv 10/Sep/2018, 2:28:22 pm (UTC)'!
test07readFromWhenTheInitialValueIncludesAnInvalidRadixItRaisesAnError
	|radixBase number initualNumber |
	
	radixBase := '0r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	
	self should: [ initualNumber asNumber ] raise: Error description: 'Invalid radix'! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/Sep/2018, 8:32:50 pm (UTC)'!
test08readFromWhenTheInitialValueIncludesANegativeRadixItConvertsTheNumberWithTheIndicatedRadixBase
	|radixBase number initualNumber |
	
	radixBase := '-2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: -30! !

!NumberTest methodsFor: 'test' stamp: 'jmv 10/Sep/2018, 2:28:25 pm (UTC)'!
test09readFromWhenTheInitialValueIsEmptyItRaisesAnError
	
	self should: [ Number readFrom: '' ] raise: Error description: 'At least one digit expected here'! !

!NumberTest methodsFor: 'support' stamp: 'GC 8/Sep/2018, 6:45:07 pm (UTC)'!
assert: expected is: actual
	^ self
		assert: (expected == actual)
		description: (self comparingStringBetween: expected and: actual)! !

!NumberTest methodsFor: 'support' stamp: 'jmv 31/Jul/2023, 3:22:43 pm (UTC)'!
assertThat: aNumber isEqualTo: anExpectedNumber

	| result |
	result := self subject: aNumber.
	
	self assert: anExpectedNumber equals: result! !

!NumberTest methodsFor: 'support' stamp: 'GC 8/Sep/2018, 6:48:11 pm (UTC)'!
subject: aString

	| aStream |
	aStream := ReadStream on: aString.

	^ Number readFrom: aStream! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 8/Aug/2024, 4:28:13 pm (UTC)'!
testBasicNew

	self should: [SmallInteger basicNew] raise: TestResult exError. ! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 4/Jan/2017, 12:16:27 pm (UTC)'!
testMaxVal

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger maxVal = 16r3FFFFFFF) ]
		ifFalse: [
			self assert: (SmallInteger maxVal = 16rFFFFFFFFFFFFFFF) ]! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 4/Jan/2017, 12:17:27 pm (UTC)'!
testMinVal

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger minVal = -16r40000000) ]
		ifFalse: [
			self assert: (SmallInteger minVal = -16r1000000000000000) ]! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 8/Aug/2024, 4:28:16 pm (UTC)'!
testNew

	self should: [SmallInteger new] raise: TestResult exError. ! !

!SmallIntegerTest methodsFor: 'testing - arithmetic' stamp: 'sd 5/Jun/2005, 12:59:00 pm (UTC)'!
testDivide

	self assert: 2 / 1 = 2.
	self assert: (3 / 2) isFraction.
	self assert: 4 / 2 = 2.
	self should: [ 1 / 0 ] raise: ZeroDivide.! !

!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 8/May/2012, 1:35:00 am (UTC)'!
testEven
	
	self assert: (SmallInteger minVal even).
	self deny: (SmallInteger maxVal even).
	
	self deny: ((SmallInteger minVal + 1) even).
	self assert: ((SmallInteger maxVal - 1) even).
	
	self deny: (1 even).
	self deny: (-1 even).
	
	self assert: (2 even).
	self assert: (-2 even).
	
	self assert: (0 even)! !

!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 8/May/2012, 1:35:00 am (UTC)'!
testOdd
	
	self deny: (SmallInteger minVal odd).
	self assert: (SmallInteger maxVal odd).
	
	self assert: ((SmallInteger minVal + 1) odd).
	self deny: ((SmallInteger maxVal - 1) odd).
	
	self assert: (1 odd).
	self assert: (-1 odd).
	
	self deny: (2 odd).
	self deny: (-2 odd).
	
	self deny: (0 odd)! !

!SmallIntegerTest methodsFor: 'testing - printing' stamp: 'jmv 10/Jan/2014, 3:41:00 am (UTC)'!
testPrintPaddedWith

	self assert: (123 printStringBase: 2 length: 10 padded: true) = '0001111011'.
	self assert: (123 printStringBase: 8 length: 10 padded: true) = '0000000173'.
	self assert: (123 printStringBase: 10 length: 10 padded: true) = '0000000123'.
	self assert: (123 printStringBase: 16 length: 10 padded: true) = '000000007B'! !

!SmallIntegerTest methodsFor: 'testing - printing' stamp: 'jmv 4/Jan/2017, 12:22:13 pm (UTC)'!
testPrintString
	self assert: 1 printString  = '1'.
	self assert: -1 printString  = '-1'.
	Smalltalk wordSize = 4
		ifTrue: [
			self assert: SmallInteger minVal printString  = '-1073741824'.
			self assert: SmallInteger maxVal printString  = '1073741823' ]
		ifFalse: [
			self assert: SmallInteger minVal printString  =  '-1152921504606846976' .
			self assert: SmallInteger maxVal printString  =  '1152921504606846975'  ].
	self assert: 12345 printString  = '12345'.
	self assert: -54321 printString  = '-54321'.

	self assert: 0 decimalDigitLength = 1.
	self assert: 4 decimalDigitLength = 1.
	self assert: 12 decimalDigitLength = 2.
	self assert: 123 decimalDigitLength = 3.
	self assert: 1234 decimalDigitLength = 4.
	self assert: 56789 decimalDigitLength = 5.
	self assert: 657483 decimalDigitLength = 6.
	self assert: 6571483 decimalDigitLength = 7.
	self assert: 65174383 decimalDigitLength = 8.
	self assert: 625744831 decimalDigitLength = 9.
	self assert: 1000001111 decimalDigitLength = 10.
	Smalltalk wordSize = 4
		ifTrue: [
			self assert: SmallInteger maxVal decimalDigitLength = 10 ]
		ifFalse: [
			self assert: SmallInteger maxVal decimalDigitLength = 19 ]! !

!SmallIntegerTest methodsFor: 'tests - zero behavior' stamp: 'jmv 2/Jul/2019, 3:50:44 pm (UTC)'!
testZeroRaisedToNegativePower
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"

	self should: [0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.

	self should: [-0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1.0] raise: ZeroDivide.! !

!SmallIntegerTest methodsFor: 'tests - printing' stamp: 'jmv 24/May/2024, 3:13:21 pm (UTC)'!
testDecimalDigitLength

	| x length random |
	"Test edge cases"
	x := 1.
	length := 1.
	[ x <= SmallInteger maxVal ] whileTrue: [
		self 
			assert: length equals: x decimalDigitLength;
			assert: (length - 1 max: 1) equals: (x - 1) decimalDigitLength.
		x := x * 10.
		length := length + 1 ].
	"A few values by hand"
	#(
		0 1
		4 1
		12 2
		123 3
		1234 4
		56789 5
		657483 6
		6571483 7
		65174383 8
		625744831 9
		1000001111 10
	), {
		SmallInteger maxVal. Smalltalk wordSize = 8 ifTrue: [ 19 ] ifFalse: [ 10 ]
	}pairsDo: [ :input :expectedOutput |
		self assert: expectedOutput equals: input decimalDigitLength ].
	"Pseudorandom tests."
	random := Random seed: 36rSQUEAK.
	10000 timesRepeat: [
		x := SmallInteger maxVal atRandom: random.
		self assert: x asString size equals: x decimalDigitLength ]! !

!SmallIntegerTest methodsFor: 'tests - arithmetic' stamp: 'nice 21/Aug/2019, 6:55:00 am (UTC)'!
testDivideMayOverflow
	"Dividing a SmallInteger by another Integer may answer a Large Integer.
	These cases have caused several VM bugs in the past, it's better to keep some assrtion around."
	
	self assert: (SmallInteger minVal / -1) isLarge.
	self assert: (SmallInteger minVal / -1) = (SmallInteger maxVal + 1).
	
	self assert: (SmallInteger minVal quo: -1) isLarge.
	self assert: (SmallInteger minVal quo: -1) = (SmallInteger maxVal + 1).
	
	self assert: (SmallInteger minVal // -1) isLarge.
	self assert: (SmallInteger minVal // -1) = (SmallInteger maxVal + 1).! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 10:33:00 pm (UTC)'!
byteBitCounts

	| tablePower answer |
	tablePower _ 8.
	answer _ ByteArray new: (1 bitShift: tablePower).
	0 to: tablePower - 1 do:
		[:eachPowerIndex |
			| powerOfTwo |
			powerOfTwo _ 1 bitShift: eachPowerIndex.
			1 + powerOfTwo to: answer size by: powerOfTwo * 2 do:
				[:eachPivot |
					0 to: powerOfTwo - 1 do:
						[:eachIndex |
							answer
								at: eachPivot + eachIndex
								put: (answer at: eachPivot + eachIndex) + 1
						]
				]
		].
	^answer! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 10:16:00 pm (UTC)'!
nSamplesByDefault

	^1000! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 9:59:00 pm (UTC)'!
rng

	^self rngClass new! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 10:01:00 pm (UTC)'!
rngClass

	self subclassResponsibility! !

!RandomTest methodsFor: 'tests' stamp: 'ul 27/Feb/2015, 5:53:00 pm (UTC)'!
testNext
	"Generate some float values, and see if they are in the [0,1) interval. Also check that the smallest and the largest values are small/large enough."

	| random min max |
	min := Float infinity.
	max := Float negativeInfinity.
	random := Random seed: 112629.
	100000 timesRepeat: [
		| next | 
		next := random next.
		next < min ifTrue: [ min := next ].
		next > max ifTrue: [ max := next ].
		self assert: next >= 0.0 description: [ 'Generated value ', next asString, ' should be non-negative.' ].
		self assert: next < 1.0 description: [ 'Generated value ', next asString, ' should be less than 1.0.' ] ].
	self assert: max > 0.9999 description: 'The largest generated value should be greater than 0.9999.'.
	self assert: min < 0.0001 description: 'The smallest generated value should be less than 0.0001.'! !

!RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 6:32:34 am (UTC)'!
testNextBits
	"Test randomBits: returns integers in the correct range."
	| random |
	random _ self rng.
	0 to: 10 do: [:n| 100 timesRepeat: [self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]].
	0 to: 100 do: [:n| self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]! !

!RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 6:27:56 am (UTC)'!
testNextBitsInvalid
	"Requesting a negative amount of random bits should raise an error."
	| random |
	random _ self rng.
	self should: [random nextBits: -1] raise: Error.
	self should: [random nextBits: -100] raise: Error! !

!RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 6:38:58 am (UTC)'!
testNextInteger
	"Test nextInteger: returns integers in the correct range."
	| random n |
	random _ self rng.
	1 to: 5 do: [:i| 100 timesRepeat: [self assert: ((random nextInteger: i) between: 1 and: i)]].
	n _ 1<<1000.
	self assert: ((random nextInteger: n) between: 1 and: n)! !

!RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 6:36:52 am (UTC)'!
testNextIntegerInvalid
	| random |
	random _ self rng.
	self should: [random nextInteger: -1] raise: Error.
	self should: [random nextInteger: -100] raise: Error.
	self should: [random nextInteger: 0] raise: Error! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 5/Mar/2016, 10:38:00 pm (UTC)'!
testRandomLargeInteger1

	| random nSamples highBound odds |
	random _ self rng.
	nSamples _ self nSamplesByDefault.
	highBound _ 1 << 64.
	odds _ 0.
	nSamples timesRepeat: [odds _ highBound atRandom: random :: bitAnd: 1 :: + odds].
	self assert: (odds * 2 - nSamples * 5) abs < nSamples  "10% max deviation"! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 5/Mar/2016, 10:47:00 pm (UTC)'!
testRandomLargeInteger2

	| random nSamples bitCounts bitsPerRandom highBound ones |
	random _ self rng.
	nSamples _ self nSamplesByDefault.
	bitCounts _ self byteBitCounts.
	bitsPerRandom _ 1000.
	highBound _ 1 << bitsPerRandom.
	ones _ 0.
	nSamples timesRepeat:
		[
			| next |
			next _ highBound atRandom: random.
			1 to: next basicSize do:
				[:eachIndex | ones _ bitCounts at: (next basicAt: eachIndex) + 1 :: + ones].
		].
	self assert: ones - (nSamples * bitsPerRandom / 2) abs / 10 < nSamples "1% max deviation"! !

!RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 7:13:26 am (UTC)'!
testSeedAndRepeatability
	| random seed a b c |
	random := self rng.
	seed _ ((1 << 100) negated to: 1 << 100) atRandom.
	random seed: seed.
	a _ random next.
	b _ random next.
	c _ random nextBits: 100.
	random seed: seed.
	self assert: random next = a.
	self assert: random next = b.
	self assert: (random nextBits: 100) = c! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 5/Mar/2016, 10:17:00 pm (UTC)'!
testSetAtRandom

	| random set nSamples sum |
	random _ self rng.
	set _ Set with: 0 with: 1.
	nSamples _ self nSamplesByDefault.
	sum _ 0.
	nSamples timesRepeat: [sum _ set atRandom: random :: + sum].
	self assert: (sum / nSamples - 0.5) abs < 0.1! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 6/Mar/2016, 12:51:00 am (UTC)'!
testSimpleBuckets
	"This is a poor test, see Knuth's TAOCP"

	| nbuckets buckets nSamples random slot |
	nbuckets _ 1000.
	buckets _ Array new: nbuckets.
	buckets atAllPut: 0.
	nSamples _ 1000.
	random _ self rng.
	nSamples * nbuckets timesRepeat:
		[
			slot := (random next * nbuckets) floor + 1.
			buckets at: slot put: (buckets at: slot) + 1
		].
	buckets do:
		[:each |
			"max ~17% deviation"
			self assert: (each - nSamples * 6) abs < nSamples
		]! !

!RandomTest class methodsFor: 'testing' stamp: 'sqr 5/Mar/2016, 9:54:00 pm (UTC)'!
isAbstract

	^self subclasses notEmpty! !

!LaggedFibonacciRandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 10:50:00 pm (UTC)'!
nSamplesByDefault

	^self rng majorLag * 100! !

!LaggedFibonacciRandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 10:00:00 pm (UTC)'!
rngClass

	^LaggedFibonacciRandom! !

!ParkMiller88RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 11:58:00 pm (UTC)'!
rngClass

	^ParkMiller88Random! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'sqr 6/Mar/2016, 12:06:00 am (UTC)'!
testParkMillerCorrectness
	"The correctness test suggested by the authors"

	| random next |
	random _ self rng seed: 1.
	10000 timesRepeat: [next _ random next].
	self assert: next * 16r7FFFFFFF = 1043618065.0! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'sqr 5/Mar/2016, 10:48:00 pm (UTC)'!
testParkMillerInitialValues

	| nSamples random samples |
	nSamples _ self nSamplesByDefault.
	random _ self rng.
	samples _ 1 to: nSamples :: collect: [:each | random next].
	self assert: (samples average - 0.5) abs < 0.1.
	self assert: samples size * 10 > nSamples! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 7:25:04 am (UTC)'!
testParkMillerSeedCornerCases
	"Test the insernal state of the Park-Miller generator is correctly set, including corner cases."
	| random m |
	m _ 2147483647.
	{1. 2. (m-1) atRandom. m-1. m-2}
		do: [:each|
			random _ self rng seed: each.
			self assert: (random instVarNamed: #seed) = each]! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'len 7/Jun/2019, 7:25:14 am (UTC)'!
testParkMillerSeedRange
	"Test the insernal state of the Park-Miller generator is correctly set, even for arbitrary user-provided seeds."
	| random m |
	m _ 2147483647.
	{1. 1000. 10000000000000000000000. -100. 0. -1. m. m-1. m-2. m+1}
		do: [:each|
			random _ self rng seed: each.
			self assert: ((random instVarNamed: #seed) between: 1 and: m - 1)]! !

!ParkMiller93RandomTest methodsFor: 'setup' stamp: 'sqr 5/Mar/2016, 11:58:00 pm (UTC)'!
rngClass

	^ParkMiller93Random! !

!BecomeTest methodsFor: 'testing' stamp: 'jmv 29/Jan/2019, 4:03:40 pm (UTC)'!
testBecome
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUpWBecome ] raise: MethodInCallStackToBecomeInvalid! !

!BecomeTest methodsFor: 'testing' stamp: 'jmv 29/Jan/2019, 4:03:42 pm (UTC)'!
testShapeMutation
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUp ] raise: MethodInCallStackToBecomeInvalid! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 9/Jul/2018, 12:03:35 am (UTC)'!
testAndAndAndAndTrueTable

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth] and: [fifth]) 
				equals: ((((first and: [second]) and: [third]) and: [ fourth ]) and: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 9/Jul/2018, 12:03:46 am (UTC)'!
testAndAndAndTrueTable

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth]) 
				equals: (((first and: [second]) and: [third]) and: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 9/Jul/2018, 12:04:00 am (UTC)'!
testAndAndTrueTable

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first and: [second] and: [third]) 
				equals: ((first and: [second]) and: [third]) ]
		forCombinationOf: 3
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 9/Jul/2018, 12:04:10 am (UTC)'!
testOrOrOrOrTrueTable

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth] or: [fifth]) 
				equals: ((((first or: [second]) or: [third]) or: [ fourth ]) or: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 9/Jul/2018, 12:04:18 am (UTC)'!
testOrOrOrTrueTable

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth]) 
				equals: (((first or: [second]) or: [third]) or: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 9/Jul/2018, 12:04:25 am (UTC)'!
testOrOrTrueTable

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first or: [second] or: [third]) 
				equals: ((first or: [second]) or: [third]) ]
		forCombinationOf: 3
		
	
	
	! !

!BooleanTest methodsFor: 'combination generation' stamp: 'HAW 8/Jul/2018, 11:51:52 pm (UTC)'!
evaluate: aBlock collectingValuesInto: values at: aPosition 
	
	aPosition = 0 
		ifTrue: [ aBlock valueWithArguments: values ]
		ifFalse: [ 
			#(true false) do: [ :value | 
				values at: aPosition put: value. 
				self evaluate: aBlock collectingValuesInto: values at: aPosition - 1 ]]! !

!BooleanTest methodsFor: 'combination generation' stamp: 'HAW 8/Jul/2018, 11:50:53 pm (UTC)'!
evaluate: aBlock forCombinationOf: aNumberOfValues

	self evaluate: aBlock collectingValuesInto: (Array new: aNumberOfValues) at: aNumberOfValues 
	! !

!BooleanTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:13:00 am (UTC)'!
testIsBoolean
	self assert: (true is: #Boolean).
	self assert: (false is: #Boolean).! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNilIfNotNil0Arg

	self assert: (5@4 ifNil: [#foo] ifNotNil: [#bar]) = #bar.
	self assert: (nil ifNil: [#foo] ifNotNil: [#bar]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNilIfNotNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNilIfNotNil1Arg

	self assert: (5@4 ifNil: [#foo] ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNil: [#foo] ifNotNil: [:a | a printString]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNilIfNotNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = '5@4'.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNil0Arg

	self assert: (5@4 ifNotNil: [#foo]) = #foo.
	self assert: (nil ifNotNil: [#foo]) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNil0ArgAsVar

	| block |
	block := [#foo].
	self assert: (5@4 ifNotNil: block) = #foo.
	self assert: (nil ifNotNil: block) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNil1Arg

	self assert: (5@4 ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString]) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNil1ArgAsVar

	| block |
	block := [:a | a printString].
	self assert: (5@4 ifNotNil: block) = '5@4'.
	self assert: (nil ifNotNil: block) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNilIfNil0Arg

	self assert: (5@4 ifNotNil: [#foo] ifNil: [#bar]) = #foo.
	self assert: (nil ifNotNil: [#foo] ifNil: [#bar]) = #bar! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNilIfNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:46:00 pm (UTC)'!
testIfNotNilIfNil1Arg

	self assert: (5@4 ifNotNil: [:a | a printString] ifNil: [#foo]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString] ifNil: [#foo]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 16/Nov/2010, 12:47:00 pm (UTC)'!
testIfNotNilIfNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = '5@4'.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 23/Oct/2019, 1:23:56 pm (UTC)'!
testErrorSignalsTheRightException

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self error: reason ]
		raise: Error
		withMessageText: reason ! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 23/Oct/2019, 1:24:03 pm (UTC)'!
testShouldNotHappenBecauseSignalsTheRightError

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self shouldNotHappenBecause: reason ]
		raise: Error
		withMessageText: self shouldNotHappenBecauseErrorMessage, reason ! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 23/Oct/2019, 1:24:11 pm (UTC)'!
testShouldNotHappenSignalsTheRightError

	self 
		should: [ self shouldNotHappen ]
		raise: Error
		withMessageText: self shouldNotHappenErrorMessage ! !

!SpecialSelectorsTest methodsFor: 'testing' stamp: 'jmv 5/Jun/2017, 4:17:20 pm (UTC)'!
testCollectionAtSymbol
	self assert: (#(1 2 3) @ #(10 20 30)) class == Array! !

!SpecialSelectorsTest methodsFor: 'testing' stamp: 'jmv 5/Jun/2017, 4:16:49 pm (UTC)'!
testNumberAtSymbol
	self assert: (1@2) class == Point! !

!WeakMessageSendTest methodsFor: 'testing' stamp: 'HAW 30/Oct/2024, 12:24:11 pm (UTC)'!
testHasArgumentGarbageWhenOneArgumentIsNil

	| message |
	
	message := WeakMessageSend receiver: self selector: #printOn: arguments: #(1).
	message arguments at: 1 put: nil.
	
	self assert: message isAnyArgumentGarbage! !

!WeakMessageSendTest methodsFor: 'testing' stamp: 'HAW 30/Oct/2024, 12:24:43 pm (UTC)'!
testHasNoArgumentGarbageWhenNoArguments

	| message |
	
	message := WeakMessageSend receiver: self selector: #printString.
	
	self deny: message isAnyArgumentGarbage! !

!WeakMessageSendTest methodsFor: 'testing' stamp: 'HAW 30/Oct/2024, 12:24:48 pm (UTC)'!
testHasNoArgumentGarbageWhenNoNilArgument

	| message |
	
	message := WeakMessageSend receiver: self selector: #printOn: arguments: #(1).
	
	self deny: message isAnyArgumentGarbage! !

!WeakMessageSendTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:44:58 am (UTC)'!
testIsMessageSend
	self assert: (WeakMessageSend new is: #MessageSend).! !

!DelayTest methodsFor: 'testing-limits' stamp: 'laza 5/Sep/2009, 12:49:00 pm (UTC)'!
testBounds
	"self run: #testBounds"
	
	self should: [Delay forMilliseconds: -1] raise: Error.
	self shouldnt: [Delay forMilliseconds: SmallInteger maxVal + 1] raise: Error.
	self shouldnt: [(Delay forMilliseconds: Float pi) wait] raise: Error. "Wait 3ms"
! !

!DelayTest methodsFor: 'testing-limits' stamp: 'jmv 9/Oct/2012, 2:17:00 am (UTC)'!
testMultiProcessWaitOnSameDelay
	"Ensure that waiting on the same delay from multiple processes raises an error"
	"
	self new testMultiProcessWaitOnSameDelay
	"
	| delay p1 p2 wasRun |
	delay := Delay forSeconds: 1.
	wasRun := false.
	p1 := [delay wait] newProcess.
	p1 priority: Processor activePriority+1.
	p1 name: 'testMultiProcessWaitOnSameDelay-1'.
	p1 resume.
	p2 := [
		self should:[delay wait] raise: Error.
		wasRun := true.
	] newProcess.
	p2 priority:  Processor activePriority+1.
	p2 name: 'testMultiProcessWaitOnSameDelay-2'.
	p2 resume.
	p1 terminate.
	p2 terminate.
	self assert: wasRun.

! !

!DelayTest methodsFor: 'testing-limits' stamp: 'ar 22/Sep/2009, 2:12:00 am (UTC)'!
testMultiSchedule
	"Ensure that scheduling the same delay twice raises an error"
	| delay |
	delay := Delay forSeconds: 1.
	delay schedule.
	self should:[delay schedule] raise: Error.
! !

!MonitorTest methodsFor: 'examples' stamp: 'md 20/Mar/2006, 1:15:00 am (UTC)'!
testExample1

	| producer1 producer2  monitor goal work counter goalReached finished |
	goal := (1 to: 1000) asOrderedCollection.
	work := OrderedCollection new.
	counter := 0.
	goalReached := false.
	finished := Semaphore new.
	monitor := Monitor new.

	producer1 := [
       [monitor critical:
             [monitor waitUntil: [counter \\5 = 0].
              goalReached or: [work add: (counter := counter + 1)].
              goalReached := counter >= goal size.
              monitor signal
            ].
           goalReached
          ]
             whileFalse.
         finished signal.
	].

	producer2 := [
         [monitor critical:
                [monitor waitWhile: [counter \\5 = 0].
                 goalReached or: [work add: (counter := counter + 1)].
                 goalReached := counter >= goal size.
                 monitor signal].
         goalReached
       ] whileFalse.
     finished signal
	].

	producer1 forkAt: Processor userBackgroundPriority.
	producer2 forkAt: Processor userBackgroundPriority.

	finished wait; wait.
	self assert: goal = work! !

!MonitorTest methodsFor: 'examples' stamp: 'md 20/Mar/2006, 1:19:00 am (UTC)'!
testExample2
	"Here is a second version that does not use a semaphore to inform the 
	forking process about termination of both forked processes"

	| producer1 producer2  monitor goal work counter goalReached activeProducers|
	goal := (1 to: 1000) asOrderedCollection.
	work := OrderedCollection new.
	counter := 0.
	goalReached := false.
	activeProducers := 0.
	monitor := Monitor new.

  producer1 :=
      [ monitor critical: [activeProducers := activeProducers + 1].
  [monitor critical:
            [monitor waitUntil: [counter \\5 = 0].
      goalReached or: [work add: (counter := counter + 1)].
     " Transcript show: 'P1  '; show: counter printString; show: '  ';
       show: activeProducers printString; cr."
      goalReached := counter >= goal size.
      monitor signal
            ].
           goalReached
          ]
             whileFalse.
         monitor critical: [activeProducers := activeProducers - 1.
        monitor signal: #finish].
 ] .

 producer2 :=
    [monitor critical: [activeProducers := activeProducers + 1].

  [monitor critical:
          [monitor waitWhile: [counter \\5 = 0].
    goalReached or: [work add: (counter := counter + 1)].
    goalReached := counter >= goal size.
    monitor signal].
         goalReached ] whileFalse.
     monitor critical: [
		activeProducers := activeProducers - 1. 
		monitor signal: #finish].
	].

	producer1 forkAt: Processor userBackgroundPriority.
	producer2  forkAt: Processor userBackgroundPriority.


	monitor critical: [
		monitor waitUntil: [activeProducers = 0 & (goalReached)]
				for: #finish.
  	].

	self assert: goal = work
! !

!MonitorTest methodsFor: 'examples' stamp: 'ul 10/Sep/2011, 8:55:00 pm (UTC)'!
testMonitorNotGainingUnwantedSignalsDuringUnwinding

	| monitor process |
	monitor := Monitor new.
	process := [ monitor critical: [ monitor wait ] ] newProcess.
	process resume.
	50 milliSeconds asDelay wait.
	process terminate.
	self assert: 1 equals: ((monitor instVarNamed: #mutex) instVarNamed: #excessSignals)! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 9/Oct/2012, 2:15:00 am (UTC)'!
testSchedulerTermination
	| process sema gotHere sema2 |
	gotHere _ false.
	sema _ Semaphore new.
	sema2 _ Semaphore new.
	process _ [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere _ true ] newProcess.
	process priority: Processor activeProcess priority.
	process name: 'testSchedulerTermination'.
	process resume.
	sema wait.
	"until process gets scheduled"
	process terminate.
	sema2 signal.
	Processor yield.
	"will give process a chance to continue and horribly screw up"
	self assert: gotHere not.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:12:51 pm (UTC)'!
testTerminationDuringNestedUnwind1
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor activeProcess suspend] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:12:59 pm (UTC)'!
testTerminationDuringNestedUnwind2
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor activeProcess suspend. 
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:33:24 pm (UTC)'!
testTerminationDuringNestedUnwind3
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor yield] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:33:38 pm (UTC)'!
testTerminationDuringNestedUnwind4
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor yield. 
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isReady.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jar 17/Jan/2022, 10:29:13 pm (UTC)'!
testTerminationDuringNestedUnwind5
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor activeProcess terminate] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jar 17/Jan/2022, 10:29:20 pm (UTC)'!
testTerminationDuringNestedUnwind6
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor activeProcess terminate.
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:33:52 pm (UTC)'!
testTerminationDuringNestedUnwind7
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 semaphore |
	x1 := x2 := x3 := x4 := false.
	semaphore := Semaphore new.
	p := 
		[
			[
				[ ] ensure: [
					[semaphore wait] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isBlocked.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 28/Apr/2021, 8:33:56 pm (UTC)'!
testTerminationDuringNestedUnwind8
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 semaphore |
	x1 := x2 := x3 := x4 := false.
	semaphore := Semaphore new.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						semaphore wait.
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isBlocked.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 6/May/2021, 4:17:31 pm (UTC)'!
testTerminationDuringUnwind
	"An illustration of the issue of process termination during unwind.
	This uses a well-behaved unwind block that we should allow to complete
	if at all possible."
	"
	self new testTerminationDuringUnwind
	"
	| unwindStarted unwindFinished p |
	unwindStarted := unwindFinished := false.
	p := [[] ensure:[
			unwindStarted := true.
			Processor yield.
			unwindFinished := true.
		]] newProcess.
	p resume.
	self deny: unwindStarted description: 'Expected unwind block not to have started yet, but it has!!'.
	Processor yield.
	self assert: unwindStarted description: 'Expected unwind block to have started by now, but it hasn''t!!'.
	self deny: unwindFinished description: 'Expected unwind block not to have finished yet, but it has!!'.
	p terminate.
	self assert: unwindFinished description: 'Expected unwind block to have successfully finished by now, but it hasn''t!!'.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 9/Oct/2012, 2:15:00 am (UTC)'!
testUnwindFromActiveProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert:(sema isSignaled).
	process := [
		sema critical:[
			self deny: sema isSignaled.
			Processor activeProcess terminate.
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromActiveProcess'.
	process resume.
	self assert: sema isSignaled! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 9/Oct/2012, 2:14:00 am (UTC)'!
testUnwindFromForeignProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: sema isSignaled.
	process := [
		sema critical:[
			self deny: sema isSignaled.
			sema wait. "deadlock"
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromForeignProcess'.
	process resume.
	self deny: sema isSignaled.
	"This is for illustration only - the BlockCannotReturn cannot 
	be handled here (it's truncated already)"
	self shouldnt: [process terminate] raise: BlockCannotReturn.
	self assert: sema isSignaled! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:36:45 pm (UTC)'!
test1AResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:00 pm (UTC)'!
test1ATerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:10 pm (UTC)'!
test1BResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:16 pm (UTC)'!
test1BTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:22 pm (UTC)'!
test1CResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:29 pm (UTC)'!
test1CTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:34 pm (UTC)'!
test1DResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:39 pm (UTC)'!
test1DTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:45 pm (UTC)'!
test2AResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:51 pm (UTC)'!
test2ATerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:37:55 pm (UTC)'!
test2BResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:00 pm (UTC)'!
test2BTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:05 pm (UTC)'!
test2CResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:10 pm (UTC)'!
test2CTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:15 pm (UTC)'!
test2DResume
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:18 pm (UTC)'!
test2DTerminate
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:22 pm (UTC)'!
test3AResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:27 pm (UTC)'!
test3ATerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #2 failed'.
	self assert: a first & a third description: 'assert #3 failed'.
	self deny: a second | a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:31 pm (UTC)'!
test3BResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:35 pm (UTC)'!
test3BTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:39 pm (UTC)'!
test3CResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:44 pm (UTC)'!
test3CTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:52 pm (UTC)'!
test3DResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:38:58 pm (UTC)'!
test3DTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 20/Sep/2023, 7:49:13 pm (UTC)'!
test4AResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self deny: a first description: 'assert #2.1 failed'.
	self deny: a second description: 'assert #2.2 failed'.
	self deny: a third description: 'assert #2.3 failed'.
	self deny: a fourth description: 'assert #2.4 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:39:08 pm (UTC)'!
test4ATerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:39:12 pm (UTC)'!
test4BResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:39:17 pm (UTC)'!
test4BTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:39:22 pm (UTC)'!
test4CResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'Aux process should have yielded and become ready'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'Aux process should have yielded before modifying state'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'Aux process should have finished running'.
	self assert: a first & a second & a third description: 'Array with wrong contents: ', a printString.
	self deny: a fourth description: 'Last element should have not been updated'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:39:27 pm (UTC)'!
test4CTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 8/Oct/2023, 9:09:57 pm (UTC)'!
test4DResume
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false with: false.
	p := [
			self aux4D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	s at: 2 put: true.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 8/Oct/2023, 9:03:54 pm (UTC)'!
test4DTerminate
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false with: false.
	p := [
			self aux4D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	s at: 2 put: true.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 4/Apr/2023, 8:35:30 pm (UTC)'!
test5ATerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a := Array new: 4 withAll: false.
	p := [
			self aux5A: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a third.
	self deny: a second | a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:26:22 pm (UTC)'!
test5BTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5B: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:26:36 pm (UTC)'!
test5CTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5C: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:26:43 pm (UTC)'!
test5DTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5D: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:26:52 pm (UTC)'!
test6ATerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6A: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a third.
	self deny: a second | a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:27:00 pm (UTC)'!
test6BTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6B: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:27:06 pm (UTC)'!
test6CTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6C: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jar 17/Jan/2022, 10:27:12 pm (UTC)'!
test6DTerminate
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6D: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:13 pm (UTC)'!
test7AResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:17 pm (UTC)'!
test7ATerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:21 pm (UTC)'!
test7BResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:25 pm (UTC)'!
test7BTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:31 pm (UTC)'!
test7CResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:35 pm (UTC)'!
test7CTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:39 pm (UTC)'!
test7DResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:44 pm (UTC)'!
test7DTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:48 pm (UTC)'!
test8AResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:53 pm (UTC)'!
test8ATerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:45:57 pm (UTC)'!
test8BResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:46:01 pm (UTC)'!
test8BTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:46:05 pm (UTC)'!
test8CResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:46:09 pm (UTC)'!
test8CTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:46:13 pm (UTC)'!
test8DResume
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'testing' stamp: 'jmv 7/Apr/2023, 2:46:17 pm (UTC)'!
test8DTerminate
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:02 pm (UTC)'!
aux1A: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux1Ainner: anArray.
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 4:51:53 pm (UTC)'!
aux1Ainner: anArray


		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				^anArray at: 1 put: true].
			anArray at: 2 put: true]
! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:08 pm (UTC)'!
aux1B: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux1Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 4:53:00 pm (UTC)'!
aux1Binner: anArray

	[ ] ensure: [
		[Processor activeProcess suspend] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:14 pm (UTC)'!
aux1C: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:18 pm (UTC)'!
aux1D: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:23 pm (UTC)'!
aux2A: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux2Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 4:55:52 pm (UTC)'!
aux2Ainner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess suspend.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:27 pm (UTC)'!
aux2B: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux2Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 4:56:22 pm (UTC)'!
aux2Binner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess suspend.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:31 pm (UTC)'!
aux2C: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess suspend.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:36 pm (UTC)'!
aux2D: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess suspend.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:40 pm (UTC)'!
aux3A: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux3Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 4:59:43 pm (UTC)'!
aux3Ainner: anArray

	[ ] ensure: [
		[Processor yield] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:45 pm (UTC)'!
aux3B: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux3Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:00:05 pm (UTC)'!
aux3Binner: anArray

	[ ] ensure: [
		[Processor yield] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:49 pm (UTC)'!
aux3C: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:53 pm (UTC)'!
aux3D: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:13:57 pm (UTC)'!
aux4A: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux4Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:33:23 pm (UTC)'!
aux4Ainner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor yield.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:14:02 pm (UTC)'!
aux4B: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux4Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:33:51 pm (UTC)'!
aux4Binner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor yield.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:14:06 pm (UTC)'!
aux4C: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor yield.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 8/Oct/2023, 9:04:58 pm (UTC)'!
aux4D: anArray sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				[ sentinel second ] whileFalse: [
					Processor yield ].
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:34:41 pm (UTC)'!
aux5A: anArray

	[
		self aux5Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:34:35 pm (UTC)'!
aux5Ainner: anArray

	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:35:04 pm (UTC)'!
aux5B: anArray

	[
		self aux5Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:34:57 pm (UTC)'!
aux5Binner: anArray

	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 2:16:05 pm (UTC)'!
aux5C: anArray

	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 2:16:13 pm (UTC)'!
aux5D: anArray

	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:35:35 pm (UTC)'!
aux6A: anArray

	[
		self aux6Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:35:29 pm (UTC)'!
aux6Ainner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess terminate.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:35:58 pm (UTC)'!
aux6B: anArray

	[
		self aux6Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:35:52 pm (UTC)'!
aux6Binner: anArray

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess terminate.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 2:16:48 pm (UTC)'!
aux6C: anArray

	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess terminate.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 2:16:56 pm (UTC)'!
aux6D: anArray

	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess terminate.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:14:53 pm (UTC)'!
aux7A: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux7Ainner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 5:37:07 pm (UTC)'!
aux7Ainner: anArray semaphore: semaphore

	[ ] ensure: [
		[semaphore wait] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:00 pm (UTC)'!
aux7B: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux7Binner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 6:23:09 pm (UTC)'!
aux7Binner: anArray semaphore: semaphore

	[ ] ensure: [
		[semaphore wait] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:03 pm (UTC)'!
aux7C: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[semaphore wait] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:07 pm (UTC)'!
aux7D: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[semaphore wait] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:12 pm (UTC)'!
aux8A: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux8Ainner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 6:24:06 pm (UTC)'!
aux8Ainner: anArray semaphore: semaphore

	[ ] ensure: [
		[] ensure: [
			semaphore wait.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:16 pm (UTC)'!
aux8B: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		self aux8Binner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 29/Apr/2021, 6:24:28 pm (UTC)'!
aux8Binner: anArray semaphore: semaphore

	[ ] ensure: [
		[] ensure: [
			semaphore wait.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:21 pm (UTC)'!
aux8C: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				semaphore wait.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true! !

!ProcessTerminateUnwindNonLocalReturn methodsFor: 'aux' stamp: 'jmv 4/Apr/2023, 8:15:25 pm (UTC)'!
aux8D: anArray semaphore: semaphore sentinel: sentinel

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				semaphore wait.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true! !

!ProcessTest methodsFor: 'tests' stamp: 'jmv 17/Aug/2022, 5:21:12 pm (UTC)'!
testAtomicSuspend
	"Test atomic suspend of foreign processes"

	| list p sema |
	sema := Semaphore new.
	p := [sema wait] newProcess.
	p resume.
	Processor yield.
	Smalltalk processSuspensionUnblocks ifFalse: [
		self shouldnt: [list := p suspendPrimitivelyOrFail] raise: Error ].
	self assert: list == nil.
! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 21/Jan/2023, 8:35:35 pm (UTC)'!
testIsTerminated
	"Test semantics of isTerminated.
	 A process is considered terminated if the suspendedContext has no unexecuted code left,
	 a situation tested by the Context >> #isDone method."
	
	"Note: If the suspended context's pc is anywhere but the last instruction, the process is NOT
	 considered terminated regardless of the status of the sender chain contexts, while on the
	 other hand if the suspended context's pc is nil, the process IS considered terminated
	 regardless of the 	status of the sender chain contexts. In all other cases the sender chain
	 is checked recursively for any unexecuted code.
	
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006822.html"
	
	| p ctx bottom |
	"let's create a test process consisting of a bottom context only:"
	ctx _ [4] asContext.
	p _ Process forContext: ctx priority: Processor activePriority.

	"initial situation: pc = startpc, sender = nil"
	self deny: p isTerminated.
	ctx pc: ctx endpc. "pc points to the last instruction now"
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's add a new bottom context..."
	bottom _ [2] asContext.
	ctx privSender: bottom.

	"... and reset ctx pc:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self deny: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's point bottom's pc to its last instruction..."
	bottom pc: bottom endpc.

	"... and reset ctx pc again:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's nil bottom's pc..."
	bottom pc: nil.

	"... and reset ctx pc one more time:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.
	
	"let's test boundary situations:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	p suspendedContext: nil.
	self assert: p isTerminated.

	"... and the active process is never considered terminated"
	self deny: Processor activeProcess isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 2/Feb/2022, 12:33:44 am (UTC)'!
testProcessFaithfulTermination
	"When terminating a process, unwind blocks should be evaluated as if they were executed by the process being terminated."

	| process |
	[
		[process _ Processor activeProcess.
		Processor activeProcess suspend] ensure: [
			self assert: process = Processor activeProcess]	
	] fork.
	Processor yield.
	process terminate.
	self assert: process isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 30/Jan/2023, 5:58:49 pm (UTC)'!
testResumeTerminatingProcess
	"An attempt to resume a terminating process in the middle of preparing its suspended context should raise an error;
	one possible solution is to nil the suspended context of the terminating process while executing the vulnerable code"
	
	"Note: another solution could be to run the vulnerable part of the code in #terminate using #valueUnpreemptively;
	currently, however, #valueUnpreemptively is affected by the #priority: bug described here: 	http://lists.squeakfoundation.org/pipermail/squeak-dev/2021-December/217473.html"

	| terminatee terminator unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ unwound not]] newProcess.
	terminatee priority: Processor activePriority + 1. 
	terminatee resume.
	terminator _ [terminatee terminate] newProcess.
	terminator priority: Processor activePriority + 1. 
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended. 
	"run terminator and stop inside #terminate"
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"resuming the terminatee process should causes the VM to raise an error while its suspended context is nil"
	self should: [terminatee resume] raise: Error.
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"now let the terminator finish its interrupted termination of the terminatee process"
	terminator resume.
	self assert: terminatee isTerminated.
	self assert: unwound.
	self assert: terminator isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'sqr 19/Jun/2024, 3:12:01 am (UTC)'!
testResumeWithEnsureAfterBCR
	"This test verifies #cannotReturn: context sender's pc has been nilled
	 before returning to make sure its code won't continue executing,
	 an unwind block will be run, and an Error will be raised.
	
	Without nilling the pc this example would crash the VM after resumption:
		[[^ 1] on: BlockCannotReturn do: [:ex | ex resume] ] fork
	
	and this example would happily execute an illegal return after resumption:
		[[true ifTrue: [^ 1]] on: BlockCannotReturn do: [:ex | ex resume] ] fork
	
	Note: push a fake stack top to prevent a VM crash when running this test in
	an image without the amended #cannotReturn:to: nilling the receiver's pc;
	the functionality of the test is not affected by this precautionary measure.	"

	| expr executedReturnTwo error unwound sync |
	expr := true.
	executedReturnTwo := false.
	error := false.
	unwound := false.
	sync := Semaphore new.
	[	[sync signal.
	      [[expr ifTrue: [thisContext push: nil. ^1].  "see the note above"
	       executedReturnTwo := true. ^2]
			ensure: [unwound := true]]
			        on: BlockCannotReturn
			        do: [:ex | ex resume]]
					on: UnhandledError
					do: [error := true]] fork.
	sync wait.
	self deny: executedReturnTwo.
	self assert: unwound.
	self assert: error! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 11:04:07 pm (UTC)'!
testRevisedSuspendExpectations
	"Test revised suspend expectations vs. pre-2022 VM's suspend"

	| sema p list |
	sema _ Semaphore new.
	p _ [sema critical:[]] newProcess.
	p priority: Processor activePriority + 1.
	p resume.
	list _ p suspend. "this is a list the process p was waiting on prior to being suspended"

	Smalltalk processSuspensionUnblocks 
		ifFalse: [ "this is the newer, post-2022 suspend semantics"
			self assert: p suspendingList isNil.
			self assert: list isNil.
			self assert: p suspendedContext selectorToSendOrSelf = #wait
			"note the list has been cleared and the pc moved back before the wait"
			]
		ifTrue: [ "this is the older, pre-2022 suspend semantics"
			self assert: p suspendingList isNil.
			self assert: list = sema.
			self deny: p suspendedContext selectorToSendOrSelf = #wait
			"note the list confirms that process p was waitng on a semaphore"
			]! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 9:56:59 pm (UTC)'!
testTerminateByHighestPriorityProcess
	"Test temination by a highest priority process."

	| p q |
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	self assert: p isBlocked.

	q _ [p terminate] newProcess.
	q priority: Processor highestPriority.
	q resume.
	self assert: p isTerminated.
	self assert: q isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 9:45:30 pm (UTC)'!
testTerminateEnsureAsStackTop
	"Test #ensure unwind block is executed even when #ensure context is on stack's top."

	| p1 p2 p3 x1 x2 x3 |
	x1 _ x2 _ x3 _ false.
	
	"p1 is at the beginning of the ensure block; the unwind block hasn't run yet"
	p1 _ [[] ensure: [x1 _ x1 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [(ctx tempAt: 2) isNil]] value: p1 suspendedContext] whileFalse: [p1 step].

	"p2 has already set complete to true (tempAt: 2) but the unwind block hasn't run yet"
	p2 _ [[] ensure: [x2 _ x2 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [(ctx tempAt: 2) notNil]] value: p2 suspendedContext] whileFalse: [p2 step].

	"p3 has already set complete to true AND the unwind block has run already run;
	we have to verify the unwind block is not executed again during termination"
	p3 _ [[] ensure: [x3 _ x3 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [ctx willReturn]] value: p3 suspendedContext] whileFalse: [p3 step].

	"make sure all processes are running and only the p3's unwind block has finished"
	self deny: p1 isTerminated | p2 isTerminated | p3 isTerminated.
	self deny: x1 | x2.
	self assert: x3. "p3 has already run its unwind block; we test it won't run it again"
	"terminate all processes and verify all unwind blocks have finished correctly"
	p1 terminate. p2 terminate. p3 terminate.
	self assert: p1 isTerminated & p2 isTerminated & p3 isTerminated.
	self assert: x1 & x2 & x3! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:01:37 pm (UTC)'!
testTerminateEnsureOnTopOfEnsure
	"Test two ensure contexts on top of each other unwind correctly, i.e. that
	both their unwind blocks get executed."
	
	| beenHere beenHereToo bottom p top |
	beenHere _ beenHereToo _ false.
	bottom _ ContextPart contextEnsure: [beenHereToo _ true].
	top _ ContextPart contextEnsure: [Processor activeProcess suspend. beenHere _ true].
	top privSender: bottom.
	p _ Process forContext: top priority: Processor activeProcess priority.
	p resume.
	Processor yield.
	self assert: p isSuspended. 
	p terminate.
	self assert: beenHere & beenHereToo.
	self assert: p isTerminated

! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 20/Nov/2025, 1:18:03 pm (UTC)'!
testTerminateEverywhere

	| block steps |
	block := [([[Notification signal]
		ensure: []]
			on: Notification do: [:ex | ex resume: 6])
				* 7].
	
	steps := 0.
	[steps] whileNotNil:
		[| process |
		process := Process forContext: block asContext priority: Processor activePriority.
		steps timesRepeat:
			[(process suspendedContext stackPtr > 0 and: [process suspendedContext top = 42])
				ifTrue: [steps := nil]
				ifFalse: [process step]].
		steps ifNotNil: [steps := steps + 1].
		"we need to skip quick returns to prevent VM crash; they are not suspension points."
		process suspendedContext method isQuick ifTrue: [process step. steps := steps + 1].
		
		process terminate.
		self assert: process isTerminated].! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:05:42 pm (UTC)'!
testTerminateHandlingUnwindError
	"Test an error inside an unwind block is handled correctly."
	
	"Workspace example:
		[ [[Processor activeProcess terminate] ensure: [1/0]] on: ZeroDivide do: [] ] fork
	
	ZeroDivide error should get caught by the handler without opening the Debugger.
	
	To model this example as a test case we have to keep in mind that an error signal 
	in one thread cannot be caught in a different thread: if process 'p' signals an error
	it won't be searching for a handler in the thread that sent 'p terminate' message.
	So we can't do something like:
		p := [ [ [Semaphore new wait] ensure: [1/0] ] on: ZeroDivide do: [] ] newProcess.
		p resume. Processor yield.
		self shouldnt: [p terminate] raise: Error
	Instead, in order to catch the situation the ZeroDivide error is not caught within 'p',
	we try to catch the UnhandledError raised in 'p' indicating the ZeroDivide has been
	missed.	"
	
	| p error unwindBlock |
	unwindBlock _ [[1/0] on: UnhandledError do: [error _ true]].
	p _ [ [ [Semaphore new wait] ensure: unwindBlock ] on: ZeroDivide do: [] ] newProcess.
	p resume. Processor yield.
	self assert: p isBlocked. 
	error _ false.
	p terminate.
	self deny: error.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:07:54 pm (UTC)'!
testTerminateHighestPriorityProcess
	"Test termination of a highest priority process."
	
	| p |
	p _ [Semaphore new wait] newProcess.
	p priority: Processor highestPriority.
	p resume.
	self assert: p isBlocked.
	p terminate.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 2:59:59 pm (UTC)'!
testTerminateInEnsure
	"As shown in
	Martin McClure's 'Threads, Critical Sections, and Termination' (Smalltalks 2019 conference)
	https://youtu.be/AvM5YrjK9AE
	at 23:17
	self new testTerminateInEnsure
	"
	| process count delay |
	10 timesRepeat: [
		process := [
			count := 0.
			[] ensure: [
				10 timesRepeat: [
					count := count + 1.
					1000000 timesRepeat: [12 factorial]].
				count := count + 1]
			] newProcess.
		process resumeAt: Processor activeProcess priority - 1.
		delay := Delay forMilliseconds: 10.
		[ count isNil ] whileTrue: [
			delay wait ].
		self assert: process isTerminated not.
		process terminate.
		process priority: Processor activeProcess priority + 1.
		self
			assert: process isTerminated;
			assert: count equals: 11 ]! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 9:34:01 pm (UTC)'!
testTerminateInTerminate
	"Terminating a terminator process should unwind both the terminator and its terminatee process"
	
	| terminator terminatee unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ true]] newProcess.
	terminator _ [terminatee terminate] newProcess.
	terminatee resume. Processor yield.
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended.
	"run terminator and stop inside #terminate" 
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminator isSuspended.
	self deny: unwound.
	terminator terminate.
	self assert: terminatee isTerminated. 
	self assert: terminator isTerminated. 
	self assert: unwound! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:14:14 pm (UTC)'!
testTerminateNiledSuspendedContextProcess
	"Test a process with niled suspendedContext terminates correctly."
	
	| p |
	p _ [] newProcess.
	self assert: p isSuspended.
	p suspendedContext: nil.
	p terminate.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 22/Jan/2023, 11:59:08 pm (UTC)'!
testTerminateProcessTerminatedViaNiledPc
	"Terminating a terminated process may still run unexecuted unwinds.
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006825.html"
	
	| terminatee unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ true]] newProcess.
	terminatee resume. Processor yield.
	self assert: terminatee isSuspended.
	self deny: unwound.
	terminatee suspendedContext pc: nil.
	self assert: terminatee isTerminated.
	self deny: unwound.
	terminatee terminate.
	self assert: terminatee isTerminated. 
	self assert: unwound! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:14:03 pm (UTC)'!
testTerminateSingleEnsure
	"Test a stack consisting of a single ensure context unwinds correctly."
	
	| beenHere p singleton |
	beenHere _ false.
	singleton _ ContextPart contextEnsure: [beenHere _ true].
	p _ Process forContext: singleton priority: Processor activeProcess priority.
	self assert: p isSuspended. 
	self assert: p suspendedContext sender isNil. 
	p terminate.
	self assert: beenHere.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 30/Jan/2023, 6:03:24 pm (UTC)'!
testTerminateTerminatingProcess
	"An attempt to terminate a terminating process in the middle of preparing its suspended context should do nothing;
	one possible solution is to nil the suspended context of the terminating process while executing the vulnerable code"
	
	"Note: another solution could be to run the vulnerable part of the code in #terminate using #valueUnpreemptively;
	currently, however, #valueUnpreemptively is affected by the #priority: bug described here: 	http://lists.squeakfoundation.org/pipermail/squeak-dev/2021-December/217473.html"

	| terminatee terminator unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ unwound not]] newProcess.
	terminatee priority: Processor activePriority + 1. 
	terminatee resume.
	terminator _ [terminatee terminate] newProcess.
	terminator priority: Processor activePriority + 1. 
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended. 
	"run terminator and stop inside #terminate"
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"terminating the terminatee process again should make no difference while its suspended context is nil"
	terminatee terminate.
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"now let the terminator finish its interrupted termination of the terminatee process"
	terminator resume.
	self assert: terminatee isTerminated.
	self assert: unwound.
	self assert: terminator isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 19/Jan/2023, 10:30:25 pm (UTC)'!
testTerminateTerminatingProcessAfterUnwindStarted
	"Terminating a terminatee process after the terminator process restarted the terminatee
	 process should unwind the terminatee process and let the terminator process terminate."
	
	"Such a situation may occur e.g. when a terminating process encounters an error and
	 opens a debugger (or calls another recovery machinery). In such case it's legitimate
	 to terminate the terminating process again (by closing the debugger or as a termination
	 by another recovery tool).

	 Note: this is a different situation than in 'testTerminateTerminatingProcess' where
	 the second termination is invoked 'too early' and may cause unpredictable outcome."
	
	| terminator terminatee unwound unwindBlock |
	unwound _ false.
	unwindBlock _ [Processor activeProcess suspend. unwound _ true].
	terminatee _ [[Semaphore new wait] ensure: unwindBlock] newProcess.
	terminatee resume. Processor yield.
	terminator _ [terminatee terminate] newProcess.
	self assert: terminatee isBlocked.
	self assert: terminator isSuspended.
	terminator resume.
	Processor yield.
	"terminator starts terminatee's unwind and gets blocked on a semaphore"
	self assert: terminator isBlocked.
	self assert: terminatee isReady.
	Processor yield.
	"terminatee resumes and stops at unwindBlock's #suspend"
	self assert: terminatee isSuspended.
	self assert: terminator isBlocked.
	terminatee terminate.
	"terminatee finishes unwinding and terminates which unblocks the terminator"
	self assert: terminatee isTerminated. 
	self assert: unwound.
	self assert: terminator isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jar 15/Aug/2022, 9:51:47 pm (UTC)'!
testTerminateWithDelayInUnwind
	"Test the process that invoked the termination of another process waits
	for the other process to finish unwinding."
	
	"Insert delay into the unwind block to force rescheduling"
	
	| delay p |
	delay _ Delay forMilliseconds: 10.
	p _ [[Processor activeProcess suspend] ensure: [delay wait]] newProcess.
	p resume.
	Processor yield.
	self assert: p isSuspended.
	p terminate.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jmv 19/Nov/2025, 1:06:37 pm (UTC)'!
testTerminateWithEmptyUnwindBlock
	"Test termination of a process containing an unwind block that is empty.
	Note: A previous test with nil as the unwind block was removed, as this is no longer supported
	See email from Jaromir to the Cuis list on 19-Nov-2025.
	
	Workspace example:
	p := [[Processor activeProcess suspend] ensure: []] newProcess.
	p resume.
	[p terminate] fork
	"
	
	| p |
	p := [[Semaphore new wait] ensure: []] newProcess.
	p priority: Processor activePriority + 1.
	p resume.
	self assert: p isBlocked.
	[p terminate] forkAt: Processor activePriority + 1.
	self assert: p isTerminated! !

!ProcessTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 3:00:04 pm (UTC)'!
testValueEnsured
	"Based on #testTerminateInEnsure
	Tweaked to raise likelihood of failure, besides testing #valueEnsured
	self new testValueEnsured
	"
	| process count delay |
	100 timesRepeat: [
		process := [
			count := 0.
			[
				10 timesRepeat: [
					count := count + 1.
					100000 timesRepeat: [12 factorial]].
				count := count + 1 ] valueEnsured
			] newProcess.
		process resumeAt: Processor activeProcess priority - 1.
		delay := Delay forMilliseconds: 10.
		[ count isNil ] whileTrue: [
			delay wait ].
		self assert: process isTerminated not.
		process terminate.
		process priority: Processor activeProcess priority + 1.
		self
			assert: process isTerminated;
			assert: count equals: 11 ]! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 22/Aug/2022, 6:38:31 pm (UTC)'!
testGrabProcessor
	"
	ProcessorTest new testGrabProcessor
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessor
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 22/Aug/2022, 6:36:30 pm (UTC)'!
testGrabProcessorOnlyForNoTimeout
	"
	ProcessorTest new testGrabProcessorOnlyForNoTimeout
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay consistentCount inconsistentCount |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 1000
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: (updates = tries)
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: (observations = tries)
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted right after #grabProcessor'.
	self
		assert: consistentCount = tries
		description: 'Should have finished without timeout every time'.
	self
		assert: inconsistentCount = 0
		description: 'Should have never timeout-ed'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 22/Aug/2022, 6:37:17 pm (UTC)'!
testGrabProcessorOnlyForTimeout
	"
	1 to: 40 do: [ :i | i print. ProcessorTest new testGrabProcessorOnlyForTimeout ]
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait consistentCount inconsistentCount delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25 							"About half the time should preempt it before it finishes"
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ delay wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40									"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 10    									"Expected value is around 25"
		description: 'It seems the low priority process finished always before the timeout'.
	self
		assert: inconsistentCount > 10   									"Expected value is around 25"
		description: 'It seems the low priority process was not preempted at the timeout'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 22/Aug/2022, 6:38:58 pm (UTC)'!
testValueUnpreemptively
	"
	ProcessorTest new testValueUnpreemptively
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] valueUnpreemptively
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'private' stamp: 'jmv 22/Aug/2022, 6:20:49 pm (UTC)'!
wasteTime: milliseconds
	| t0 |
	t0 _ Time localMillisecondClock.
	[ t0 + milliseconds > Time localMillisecondClock ] whileTrue: [
		1.23 asString asNumber ]! !

!SemaphoreTest methodsFor: 'private' stamp: 'tlk 5/May/2006, 5:26:00 pm (UTC)'!
criticalError
	Processor activeProcess terminate! !

!SemaphoreTest methodsFor: 'testing' stamp: 'tlk 5/May/2006, 5:27:00 pm (UTC)'!
testCritical
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled! !

!SemaphoreTest methodsFor: 'testing' stamp: 'tlk 5/May/2006, 5:28:00 pm (UTC)'!
testCriticalIfError
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError ifError:[]]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:58:07 pm (UTC)'!
testSemaAfterCriticalWait
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical: []] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:58:19 pm (UTC)'!
testSemaAfterCriticalWait2
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p beenHere |
	s := Semaphore new. beenHere := false.
	p := [s critical: [beenHere := true]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
	self deny: beenHere.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:58:28 pm (UTC)'!
testSemaAfterCriticalWaitInEnsure
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [[] ensure: [s critical: []]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:58:41 pm (UTC)'!
testSemaAfterCriticalWaitInEnsure2
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p beenHere |
	s := Semaphore new. beenHere := false.
	p := [[] ensure: [s critical: [beenHere := true]]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
	self assert: beenHere.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:58:51 pm (UTC)'!
testSemaAfterCriticalWaitSuspended
	"Test whether a semaphore that has just left the wait in Semaphore>>critical: but 	has
	 been suspended before termination, leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical:[]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[p suspendingList == s] whileFalse: [(Delay forMilliseconds: 10) wait].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	self assert: p suspendingList class == LinkedList.
	p suspend.
	self assert: p suspendingList == nil.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:59:00 pm (UTC)'!
testSemaInCriticalEnsureArgument	
	"Test whether a process that is in an ensure argument block but has yet to evaluate the signal
	leaves it with signaling the associated semaphore."
	
	| terminatee sema |
	sema := Semaphore forMutualExclusion.
	terminatee := [sema critical: []] newProcess.
	self assert: terminatee isSuspended.
	[terminatee suspendedContext selectorToSendOrSelf = #signal] whileFalse: [terminatee step].
	self assert: terminatee isSuspended.
	terminatee terminate.
	self assert: terminatee isTerminated. 
	self assert: (sema instVarNamed: #excessSignals) equals: 1.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:59:16 pm (UTC)'!
testSemaInCriticalWait
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p |
	sentinel := Array with: false.
	s := Semaphore new.
	p := [
		sentinel at: 1 put: true.
		s critical: []] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:59:38 pm (UTC)'!
testSemaInCriticalWait2
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p beenHere |
	sentinel := Array with: false.
	s := Semaphore new. beenHere := false.
	p := [
		sentinel at: 1 put: true.
		s critical: [beenHere := true]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.
	self deny: beenHere.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:59:52 pm (UTC)'!
testSemaInCriticalWaitInEnsure
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p |
	sentinel := Array with: false.
	s := Semaphore new.
	p := [[] ensure: [
			sentinel at: 1 put: true.
			s critical: []]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 4:00:12 pm (UTC)'!
testSemaInCriticalWaitInEnsure2
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p beenHere |
	sentinel := Array with: false.
	s := Semaphore new. beenHere := false.
	p := [[] ensure: [
			sentinel at: 1 put: true.
			s critical: [beenHere := true]]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.
	self deny: beenHere.! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jar 22/Jan/2023, 12:55:46 am (UTC)'!
testSuspendAndResume
	"Test the semantics of suspending and resuming a process blocked on a semaphore;
	Note the difference between #suspend and #suspendAndUnblock.
	Note: this test will fail when run with older VMs without primitive suspend 578."

	| p |
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	p suspend; resume.
	"suspend removes p from the semaphore but backs up its pc;
	hence when resumed p reenters the wait state"
	Processor yield.

	"VMs older than 2022, where #processSuspensionUnblocks answers true,
	have this incorrect behavior (primitive 88 instead of the corrected 578)"
	Smalltalk processSuspensionUnblocks
		ifTrue: [self assert: p isTerminated]
		ifFalse: [	
			self assert: p isBlocked.
			p terminate].
	
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	p suspendAndUnblock; resume.
	"suspendAndUnblock just removes p from the semaphore;
	hence when resumed p simply proceeds"
	Processor yield.
	self assert: p isTerminated! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 5/Nov/2011, 2:12:00 am (UTC)'!
testWaitAndWaitTimeoutTogether
	| semaphore value waitProcess waitTimeoutProcess |
	semaphore := Semaphore new.
	
	waitProcess := [semaphore wait. value := #wait] newProcess.
	waitProcess resume.

	waitTimeoutProcess := [semaphore waitTimeoutMSecs: 50. value := #waitTimeout] newProcess.
	waitTimeoutProcess resume.

	"Wait for the timeout to happen"
	(Delay forMilliseconds: 100) wait.

	"The waitTimeoutProcess should already have timed out.  This should release the waitProcess"
	semaphore signal.

	[waitProcess isTerminated and: [waitTimeoutProcess isTerminated]]
		whileFalse: [(Delay forMilliseconds: 100) wait].

	self assert: value = #wait.
	! !

!SemaphoreTest methodsFor: 'testing' stamp: 'ar 23/Mar/2009, 9:01:00 pm (UTC)'!
testWaitTimeoutMSecs
	"Ensure that waitTimeoutMSecs behaves properly"

	"Ensure that a timed out waitTimeoutMSecs: returns true from the wait"
	self assert: (Semaphore new waitTimeoutMSecs: 50) == true.

	"Ensure that a signaled waitTimeoutMSecs: returns false from the wait"
	self assert: (Semaphore new signal waitTimeoutMSecs: 50) == false.
! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 7/Oct/2010, 2:08:00 am (UTC)'!
testCaseConversion
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' collect: [ :c | c asLowercase ]) = 'año comé tomá camión'.
	self assert:('año Comé tomá Camión' collect: [ :c | c asUppercase ]) = 'AÑO COMÉ TOMÁ CAMIÓN'! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 9/Jan/2024, 4:43:41 pm (UTC)'!
testCompareIsTransitive
	| c1 c2 c3 |
	1 to: 512 by: 8 do: [ :n1 |
		c1 := Character codePoint: n1.
		1 to: 512 by: 8 do: [ :n2 |
			c2 := Character codePoint: n2.
			1 to: 512 by: 8 do: [ :n3 |
				c3 := Character codePoint: n3.
				(c1 < c2) & (c2 < c3) ifTrue: [ self assert: (c1 < c3) ]]]]! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 5/Jul/2022, 7:56:54 pm (UTC)'!
testIsCharacter
	self assert: $a isCharacter.
	self assert: $X isCharacter.
	self assert: Character cr isCharacter.! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 9/Jan/2024, 4:55:01 pm (UTC)'!
testSorting
	"
	self new testSorting
	"
	self assert: $a < $á.
	self deny: $a < $Á.
	self assert: $A < $á.
	self assert: $A < $Á.
	self assert: $á < $b.
	self deny: $á < $B.
	self assert: $Á < $b.
	self assert: $Á < $B.! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 7/Oct/2010, 2:10:00 am (UTC)'!
testTestingMethods
	"
	self new testTestingMethods
	"
	self assert:
		('año Comé tomá Camión' allSatisfy: [ :c |
			c = $  or: [ c isLetter ]]).
	self assert:
		('año comé tomá camión' allSatisfy: [ :c |
			c = $  or: [ c isLowercase ]]).
	self assert:
		('AÑO COMÉ TOMÁ CAMIÓN' allSatisfy: [ :c |
			c = $  or: [ c isUppercase ]]).
	self assert:
		('AaÀàÁáÂâÃãÄäÅåEeÈèÉéÊêËëIiÌìÍíÎîÏïOoÒòÓóÔôÕõÖöUuÙùÚúÛûÜü' allSatisfy: [ :c |
			c isVowel ]).! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 26/May/2022, 4:31:12 pm (UTC)'!
testFromUtf8
	"
	CharacterTest new testFromUtf8
	"
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: '24') into: nil into: [ :codePoint | self assert: codePoint hex = '16r24' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'C2A2') into: nil into: [ :codePoint | self assert: codePoint hex = '16rA2' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'E282AC') into: nil into: [ :codePoint | self assert: codePoint hex = '16r20AC' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'F0A4ADA2') into: nil into: [ :codePoint | self assert: codePoint hex = '16r24B62' ].! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 5/Jul/2022, 8:43:46 pm (UTC)'!
testSomeLatinCharsFromUtf8
	"
	CharacterTest new testSomeLatinCharsFromUtf8
	"
	| bytes string |
	bytes _ ByteArray readHexFrom: 'C3A141C3A541C3A641C3B141C38641C2A541C3BC'.
	string _ String streamContents: [ :strm | | s byteIndex n |
		s _ bytes size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes
				into: nil
				into: [ :codePoint |
					strm nextPut: (Character codePoint: codePoint) ].
			byteIndex _ byteIndex + n
		]].
	self assert: string = 'áAåAæAñAÆA¥Aü'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 9/Jan/2024, 4:54:18 pm (UTC)'!
testSomeLatinCharsToUtf8
	"
	CharacterTest new testSomeLatinCharsToUtf8
	"
	| characters bytes |
	characters := 'áAåAæAñAÆA¥Aü' readStream.
	bytes := ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOf: characters next codePoint ]].
	self assert: bytes hex = 'C3A141C3A541C3A641C3B141C38641C2A541C3BC'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 9/Jan/2024, 4:55:20 pm (UTC)'!
testToUtf8
	"
	CharacterTest new testToUtf8
	"
	self assert: (Character utf8BytesOf: 16r0024) hex =  '24'.
	self assert: (Character utf8BytesOf: 16r00A2) hex =  'C2A2'.
	self assert: (Character utf8BytesOf: 16r20AC) hex = 'E282AC'.
	self assert: (Character utf8BytesOf: 16r024B62) hex = 'F0A4ADA2'! !

!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 8/Aug/2024, 9:23:12 pm (UTC)'!
testAsUtf8
	"
	 StringTest new testAsUtf8
	"
	| subject |
	subject := '¿Ñandú, agüita?' asByteString.
	self assert: subject asUtf8Bytes hex = 'C2BFC391616E64C3BA2C206167C3BC6974613F'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.! !

!StringTest methodsFor: 'tests - converting' stamp: 'jmv 6/Jul/2022, 1:05:36 am (UTC)'!
testBase64

	| bytes |
	bytes _ ('Hello World' collect: [ :char | char asciiValue ]) asByteArray.
	self 
		assert: 'SGVsbG8gV29ybGQ=' base64Decoded = bytes;
		assert: bytes base64Encoded = 'SGVsbG8gV29ybGQ=';
		assert: (String new: 100 withAll: $x) asByteArray base64Encoded  =
'eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4
eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eA=='! !

!StringTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:38:26 pm (UTC)'!
testBeginsWith

	self assert: ('Elvis' asByteString beginsWith: 'El' asByteString).
	self deny: ('Elvis' asByteString beginsWith: 'Paul' asByteString).
	self assert: ('Elvis' asByteString beginsWith: 'El' asUnicodeString).
	self deny: ('Elvis' asByteString beginsWith: 'Paul' asUnicodeString).
	self assert: ('Elvis' asUnicodeString beginsWith: 'El' asByteString).
	self deny: ('Elvis' asUnicodeString beginsWith: 'Paul' asByteString).
	self assert: ('Elvis' asUnicodeString beginsWith: 'El' asUnicodeString).
	self deny: ('Elvis' asUnicodeString beginsWith: 'Paul' asUnicodeString).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 7/Oct/2010, 2:08:00 am (UTC)'!
testCaseConversion
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' asLowercase) = 'año comé tomá camión'.
	self assert: ('año Comé tomá Camión' asUppercase) = 'AÑO COMÉ TOMÁ CAMIÓN'! !

!StringTest methodsFor: 'testing' stamp: 'jmv 1/Sep/2009, 6:12:00 pm (UTC)'!
testEncompassParagraph1

	self assert: ('a' encompassParagraph: (1 to: 0)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (1 to: 1)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (2 to: 1)) = (1 to: 1).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 1/Sep/2009, 6:13:00 pm (UTC)'!
testEncompassParagraph2

	self assert: ('ab' encompassParagraph: (1 to: 0)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (3 to: 2)) = (1 to: 2).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 1/Sep/2009, 6:14:00 pm (UTC)'!
testEncompassParagraph3

self assert: ('a
' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (3 to: 2)) = (3 to: 2).

self assert: ('a
zcxv' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
zcxv' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (3 to: 2)) = (3 to: 6).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 1/Sep/2009, 6:17:00 pm (UTC)'!
testEncompassParagraph4

self assert: ('
b' encompassParagraph: (1 to: 0)) = (1 to: 1).
self assert: ('
b' encompassParagraph: (1 to: 1)) = (1 to: 1).
self assert: ('
b' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('
b' encompassParagraph: (2 to: 1)) = (2 to: 2).
self assert: ('
b' encompassParagraph: (2 to: 2)) = (2 to: 2).
self assert: ('
b' encompassParagraph: (3 to: 2)) = (2 to: 2).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 1/Sep/2009, 6:22:00 pm (UTC)'!
testEncompassParagraph5

self assert: ('a
b' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 3)) = (1 to: 3).
self assert: ('a
b' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (2 to: 3)) = (1 to: 3).
self assert: ('a
b' encompassParagraph: (3 to: 2)) = (3 to: 3).
self assert: ('a
b' encompassParagraph: (3 to: 3)) = (3 to: 3).
self assert: ('a
b' encompassParagraph: (4 to: 3)) = (3 to: 3).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:24:44 pm (UTC)'!
testFindSelector 

	self assert: '"self printOn:' findSelector equals: #printOn:.
	self assert: 'self printOn:"' findSelector equals: #printOn:.
	self assert: '"self printOn:"' findSelector equals: #printOn:.
	self assert: 'self printOn:' findSelector equals: #printOn:.
! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:25:19 pm (UTC)'!
testFirstNonSeparator

	self assert: 'abc' firstNonSeparator equals: 1.
	self assert: ' abc' firstNonSeparator equals: 2.
	self assert: '' firstNonSeparator equals: 0.! !

!StringTest methodsFor: 'testing' stamp: 'jmv 5/Jul/2022, 8:02:31 pm (UTC)'!
testIsString
	self assert: ('Hello World' isString).
	self assert: ('' isString).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:26:24 pm (UTC)'!
testLastNonSeparator

	self assert: 'abc' lastNonSeparator equals: 3.
	self assert: 'abc ' lastNonSeparator equals: 3.
	self assert: ' abc ' lastNonSeparator equals: 4.
	self assert: '' lastNonSeparator equals: 0! !

!StringTest methodsFor: 'testing' stamp: 'jmv 17/Dec/2012, 2:51:00 pm (UTC)'!
testLineSeparators
	"
	Test that #newLineCharacter is considered a line separator and not a line terminator.
	This means that the last line never ends with a #newLineCharacter (although it might be empty!!)
	StringTest new testLineSeparators
	"
	| justAnLf linesBounds |
	linesBounds _ OrderedCollection new.
	justAnLf _ '
'.
	justAnLf lineIndicesDo: [ :start :endWithoutDelimiters :end |
		linesBounds add: { start . endWithoutDelimiters. end }.
		].

	self assert: linesBounds size = 2 description: 'There should be two lines.'.

	self assert: linesBounds first first = 1 description: 'First line starts at position 1'.
	self assert: linesBounds first second = (linesBounds first first-1) description: 'First line is empty'.
	self assert: linesBounds first third = (linesBounds first second+1) description: 'First line is terminated by ab Lf'.

	self assert: linesBounds second first = ( linesBounds first third+1) description: 'Second line starts after end of first line'.
	self assert: linesBounds second second = (linesBounds second first-1) description: 'Second line is empty'.
	self assert: linesBounds second third = (linesBounds second second+0) description: 'Second line is not terminated by ab Lf'.! !

!StringTest methodsFor: 'testing' stamp: 'jmv 5/Jul/2022, 8:42:37 pm (UTC)'!
testSorting
"
self new testSorting
"

	self assert: 'a' asByteString < 'á' asByteString.
	self deny: 'a' asByteString < 'Á' asByteString.
	self assert: 'A' asByteString < 'á' asByteString.
	self assert: 'A' asByteString < 'Á' asByteString.
	self assert: 'á' asByteString < 'b' asByteString.
	self deny: 'á' asByteString < 'B' asByteString.
	self assert: 'Á' asByteString < 'b' asByteString.
	self assert: 'Á' asByteString < 'B' asByteString.

	self assert: ('a' asByteString caseInsensitiveLessOrEqual: 'á' asByteString).
	self assert: ('a' asByteString caseInsensitiveLessOrEqual: 'Á' asByteString).
	self assert: ('A' asByteString caseInsensitiveLessOrEqual: 'á' asByteString).
	self assert: ('A' asByteString caseInsensitiveLessOrEqual: 'Á' asByteString).
	self assert: ('á' asByteString caseInsensitiveLessOrEqual: 'b' asByteString).
	self assert: ('á' asByteString caseInsensitiveLessOrEqual: 'B' asByteString).
	self assert: ('Á' asByteString caseInsensitiveLessOrEqual: 'b' asByteString).
	self assert: ('Á' asByteString caseInsensitiveLessOrEqual: 'B' asByteString).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:28:24 pm (UTC)'!
testWithBlanksTrimmed

	self assert: ' abc' withBlanksTrimmed equals: 'abc'.
	self assert: 'abc ' withBlanksTrimmed equals: 'abc'.
	self assert: ' abc ' withBlanksTrimmed equals: 'abc'.
	self assert: ' a b c ' withBlanksTrimmed equals: 'a b c'.
	self assert: '' equals: '' withBlanksTrimmed.
	! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:28:59 pm (UTC)'!
testWithoutLeadingBlanks

	self assert: ' abc' withoutLeadingBlanks equals: 'abc'.
	self assert: 'abc' withoutLeadingBlanks equals: 'abc'.
	self assert: '' withoutLeadingBlanks isEmpty
! !

!StringTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:38:50 pm (UTC)'!
testWithoutPrefix

	self assert: ('Elvis' asByteString withoutPrefix: 'El' asByteString) = 'vis'.
	self assert: ('Elvis' asByteString withoutPrefix: 'Paul' asByteString) isNil.
	self assert: ('Elvis' asByteString withoutPrefix: 'El' asUnicodeString) = 'vis'.
	self assert: ('Elvis' asByteString withoutPrefix: 'Paul' asUnicodeString) isNil.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'El' asByteString) = 'vis'.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'Paul asByteString') isNil.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'El' asUnicodeString) = 'vis'.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'Paul' asUnicodeString) isNil.! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:29:37 pm (UTC)'!
testWithoutSeparators

	self assert: 'abc' withoutSeparators equals: 'abc'.
	self assert: ' a b c ' withoutSeparators equals: 'abc'! !

!StringTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:30:04 pm (UTC)'!
testWithoutTrailingBlanks

	self assert: 'abc ' withoutTrailingBlanks equals: 'abc'.
	self assert: 'abc' withoutTrailingBlanks equals: 'abc'.
	self assert: '' withoutTrailingBlanks isEmpty
! !

!StringTest methodsFor: 'tests - grammar' stamp: 'LC 5/Jul/2020, 8:47:10 pm (UTC)'!
testArticle
	self
		assert: 'euphemism' withArticle equals: 'a euphemism';
		assert: 'European' withArticle equals: 'a European';
		assert: 'Euclidean space' withArticle equals: 'a Euclidean space';
		assert: 'university' withArticle equals: 'a university';
		assert: 'anagram' withArticle equals: 'an anagram';
		assert: 'apple' withArticle equals: 'an apple';
		assert: 'Ukranian' withArticle equals: 'a Ukranian';
		assert: 'Argentine' withArticle equals: 'an Argentine';
		assert: 'user' withArticle equals: 'a user'! !

!StringTest methodsFor: 'tests - padding' stamp: 'HAW 10/May/2025, 9:56:43 pm (UTC)'!
testPaddingCenterAddCharsToBothSides

	self 
		assert: ('hi' padded: #center to: 4 with: $.)
		equals: '.hi.'.
		
	self 
		assert: ('hi' padded: #center to: 5 with: $.)
		equals: '.hi..'.! !

!StringTest methodsFor: 'tests - padding' stamp: 'HAW 10/May/2025, 9:55:43 pm (UTC)'!
testPaddingLeftAddCharsToTheLeft

	self 
		assert: ('hi' padded: #left to: 4 with: $.)
		equals: '..hi'.
		
	self 
		assert: ('hi' padded: #left to: 1 with: $.)
		equals: 'hi'.! !

!StringTest methodsFor: 'tests - padding' stamp: 'HAW 10/May/2025, 9:55:59 pm (UTC)'!
testPaddingRightAddCharsToTheLeft

	self 
		assert: ('hi' padded: #right to: 4 with: $.)
		equals: 'hi..'.
		
	self 
		assert: ('hi' padded: #right to: 1 with: $.)
		equals: 'hi'.! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 29/Mar/2020, 11:59:47 pm (UTC)'!
testIsNotValidBinarySelectorIfItContainsACharacterNotAllowed

	self deny: #'(' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 30/Mar/2020, 12:01:07 am (UTC)'!
testIsNotValidKeywordSelectorIfItContainsSeparators

	self deny: #'between: and:' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 29/Mar/2020, 11:52:35 pm (UTC)'!
testIsNotValidUnarySelectorIfItContainsCharactersUsedInBinarySelectors

	self deny: #'a+b' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 29/Mar/2020, 11:51:29 pm (UTC)'!
testIsNotValidUnarySelectorIfItStartsWithALetterAndContainsSeparators

	self deny: #'with spaces' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 29/Mar/2020, 11:53:43 pm (UTC)'!
testIsValidBinarySelectorIfItContainsAnAllowedCharacterOrSequenceOfCharacters

	self
		assert: #+ isValidSelector;
		assert: #+-+ isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 30/Mar/2020, 12:00:35 am (UTC)'!
testIsValidKeywordSelectorIfItContainsKeywordsWithColons

	self
		assert: #includes: isValidSelector;
		assert: #between:and: isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 29/Mar/2020, 11:50:31 pm (UTC)'!
testIsValidUnarySelectorIfItStartsWithALetterAndDoesNotContainSeparators

	self assert: #t234 isValidSelector! !

!SymbolTest methodsFor: 'tests - symbol consistency' stamp: 'sqr 30/Dec/2021, 1:01:46 pm (UTC)'!
testNoDuplicateSymbols

	| symbols |
	symbols := Symbol allInstances sort.
	1 to: symbols size - 1 do:
		[:k |
			self deny: ((symbols at: k) hasEqualElements: (symbols at: k + 1))
		]! !

!UnicodeNormalizationTest methodsFor: 'tests' stamp: 'jmv 11/Dec/2022, 11:35:18 pm (UTC)'!
testCanonicalComposites
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r00C5)) canonicalDecomposition asCodePoints asArray = #(16r0041 16r030A).
	self assert: (UnicodeString fromCodePoints: #(16r00C5)) canonicalComposition asCodePoints asArray = #(16r00C5).

	self assert: (UnicodeString fromCodePoints: #(16r00F4)) canonicalDecomposition asCodePoints asArray = #(16r006F 16r0302).
	self assert: (UnicodeString fromCodePoints: #(16r00F4)) canonicalComposition asCodePoints asArray = #(16r00F4).! !

!UnicodeNormalizationTest methodsFor: 'tests' stamp: 'jmv 11/Dec/2022, 11:35:28 pm (UTC)'!
testMultipleCombiningMarks
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r1E69)) canonicalDecomposition asCodePoints asArray = #(16r0073 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r1E69)) canonicalComposition asCodePoints asArray = #(16r1E69).

	self assert: (UnicodeString fromCodePoints: #(16r1E0B 16r0323)) canonicalDecomposition asCodePoints asArray = #(16r0064 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r1E0B 16r0323)) canonicalComposition asCodePoints asArray = #(16r1E0D 16r0307).

	self assert: (UnicodeString fromCodePoints: #(16r0071 16r0307 16r0323)) canonicalDecomposition asCodePoints asArray = #(16r0071 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r0071 16r0307 16r0323)) canonicalComposition asCodePoints asArray = #(16r0071 16r0323 16r0307).! !

!UnicodeNormalizationTest methodsFor: 'tests' stamp: 'jmv 11/Dec/2022, 11:35:36 pm (UTC)'!
testSingletons
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r212B)) canonicalDecomposition asCodePoints asArray = #(16r0041 16r030A).
	self assert: (UnicodeString fromCodePoints: #(16r212B)) canonicalComposition asCodePoints asArray = #(16r00C5).

	self assert: (UnicodeString fromCodePoints: #(16r2126)) canonicalDecomposition asCodePoints asArray = #(16r03A9).
	self assert: (UnicodeString fromCodePoints: #(16r2126)) canonicalComposition asCodePoints asArray = #(16r03A9).! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:39:26 pm (UTC)'!
test01

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!!' asUnicodeString.

	self assert: asciiString hash = asciiUtf8String hash.
	self assert: asciiString = asciiUtf8String.
	self assert: asciiUtf8String = asciiString.

	self assert: latinString hash = latinUtf8String hash.
	self assert: latinString = latinUtf8String.
	self assert: latinUtf8String = latinString.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:40:08 pm (UTC)'!
test02

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!!' asUnicodeString.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:40:12 pm (UTC)'!
test03

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!!' asUnicodeString.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:40:20 pm (UTC)'!
test04

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!!' asUnicodeString.
	asciiString _ asciiString asLowercase.
	latinString _ latinString asLowercase.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:16:22 pm (UTC)'!
test05Utf8

	self deny: (UnicodeString fromUtf8Bytes: #[64 65 66]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 32]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 9]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 10]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 13]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 12]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 194 133]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 168 ]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 169 ]) isLastSeparator.

	self deny: (UnicodeString fromUtf8Bytes: #[64 65 66]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 10]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 13]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 12]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 194 133]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 168 ]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 169 ]) isLastLineSeparator.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:41:21 pm (UTC)'!
test06isAscii

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!!' asUnicodeString.

	self assert: asciiString isAscii.
	self assert: asciiUtf8String isAscii.
	self deny: latinString isAscii.
	self deny: latinUtf8String isAscii.

	self assert: asciiString asLowercase isAscii.
	self assert: asciiUtf8String asLowercase isAscii.
	self deny: latinString asLowercase isAscii.
	self deny: latinUtf8String asLowercase isAscii.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:42:37 pm (UTC)'!
test07
	| s |

	self assert: ('Hello World' asByteString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.
	self assert: ('Hello World' asUnicodeString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.
	self assert: ('Hello World' asUnicodeString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.

	s _ '¿El Ñandú toma agüita?' asByteString.
	self assert: (s copyReplaceFrom: 3 to: 4 with: 'xxx') =  '¿ExxxÑandú toma agüita?'.
	self assert: (s copyReplaceFrom: 3 to: 2 with: 'xxx') =  '¿Exxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: 1 to: 2 with: 'xxx') =  'xxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: s size to: s size with: 'xxx') =  '¿El Ñandú toma agüitaxxx'.
	self assert: (s copyReplaceFrom: s size to: 2 with: 'xxx') =  '¿El Ñandú toma agüitaxxxl Ñandú toma agüita?'.

	s _ '¿El Ñandú toma agüita?' asUnicodeString.
	self assert: (s copyReplaceFrom: 3 to: 4 with: 'xxx') =  '¿ExxxÑandú toma agüita?'.
	self assert: (s copyReplaceFrom: 3 to: 2 with: 'xxx') =  '¿Exxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: 1 to: 2 with: 'xxx') =  'xxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: s size to: s size with: 'xxx') =  '¿El Ñandú toma agüitaxxx'.
	self assert: (s copyReplaceFrom: s size to: 2 with: 'xxx') =  '¿El Ñandú toma agüitaxxxl Ñandú toma agüita?'.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 10/Jun/2022, 8:06:56 pm (UTC)'!
test08a
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!!"

	| asciiString0 asciiString1 asciiString2 |
	asciiString0 _ 'Helle world!!'.
	asciiString1 _ 'Hello world'.
	asciiString2 _ 'Hellu world!!'.

	self deny: asciiString0 < asciiString0.
	self assert: asciiString0 < asciiString1.
	self assert: asciiString0 < asciiString2.
	self deny: asciiString1 < asciiString0.
	self deny: asciiString1 < asciiString1.
	self assert: asciiString1 < asciiString2.
	self deny: asciiString2 < asciiString0.
	self deny: asciiString2 < asciiString1.
	self deny: asciiString2 < asciiString2.

	self assert: asciiString0 <= asciiString0.
	self assert: asciiString0 <= asciiString1.
	self assert: asciiString0 <= asciiString2.
	self deny: asciiString1 <= asciiString0.
	self assert: asciiString1 <= asciiString1.
	self assert: asciiString1 <= asciiString2.
	self deny: asciiString2 <= asciiString0.
	self deny: asciiString2 <= asciiString1.
	self assert: asciiString2 <= asciiString2.

	self deny: asciiString0 > asciiString0.
	self deny: asciiString0 > asciiString1.
	self deny: asciiString0 > asciiString2.
	self assert: asciiString1 > asciiString0.
	self deny: asciiString1 > asciiString1.
	self deny: asciiString1 > asciiString2.
	self assert: asciiString2 > asciiString0.
	self assert: asciiString2 > asciiString1.
	self deny: asciiString2 > asciiString2.

	self assert: asciiString0 >= asciiString0.
	self deny: asciiString0 >= asciiString1.
	self deny: asciiString0 >= asciiString2.
	self assert: asciiString1 >= asciiString0.
	self assert: asciiString1 >= asciiString1.
	self deny: asciiString1 >= asciiString2.
	self assert: asciiString2 >= asciiString0.
	self assert: asciiString2 >= asciiString1.
	self assert: asciiString2 >= asciiString2.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:42:49 pm (UTC)'!
test08b
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiString0 < asciiUtf8String0.
	self assert: asciiString0 < asciiUtf8String1.
	self assert: asciiString0 < asciiUtf8String2.
	self deny: asciiString1 < asciiUtf8String0.
	self deny: asciiString1 < asciiUtf8String1.
	self assert: asciiString1 < asciiUtf8String2.
	self deny: asciiString2 < asciiUtf8String0.
	self deny: asciiString2 < asciiUtf8String1.
	self deny: asciiString2 < asciiUtf8String2.

	self assert: asciiString0 <= asciiUtf8String0.
	self assert: asciiString0 <= asciiUtf8String1.
	self assert: asciiString0 <= asciiUtf8String2.
	self deny: asciiString1 <= asciiUtf8String0.
	self assert: asciiString1 <= asciiUtf8String1.
	self assert: asciiString1 <= asciiUtf8String2.
	self deny: asciiString2 <= asciiUtf8String0.
	self deny: asciiString2 <= asciiUtf8String1.
	self assert: asciiString2 <= asciiUtf8String2.

	self deny: asciiString0 > asciiUtf8String0.
	self deny: asciiString0 > asciiUtf8String1.
	self deny: asciiString0 > asciiUtf8String2.
	self assert: asciiString1 > asciiUtf8String0.
	self deny: asciiString1 > asciiUtf8String1.
	self deny: asciiString1 > asciiUtf8String2.
	self assert: asciiString2 > asciiUtf8String0.
	self assert: asciiString2 > asciiUtf8String1.
	self deny: asciiString2 > asciiUtf8String2.

	self assert: asciiString0 >= asciiUtf8String0.
	self deny: asciiString0 >= asciiUtf8String1.
	self deny: asciiString0 >= asciiUtf8String2.
	self assert: asciiString1 >= asciiUtf8String0.
	self assert: asciiString1 >= asciiUtf8String1.
	self deny: asciiString1 >= asciiUtf8String2.
	self assert: asciiString2 >= asciiUtf8String0.
	self assert: asciiString2 >= asciiUtf8String1.
	self assert: asciiString2 >= asciiUtf8String2.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:43:01 pm (UTC)'!
test08c
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiUtf8String0 < asciiString0.
	self assert: asciiUtf8String0 < asciiString1.
	self assert: asciiUtf8String0 < asciiString2.
	self deny: asciiUtf8String1 < asciiString0.
	self deny: asciiUtf8String1 < asciiString1.
	self assert: asciiUtf8String1 < asciiString2.
	self deny: asciiUtf8String2 < asciiString0.
	self deny: asciiUtf8String2 < asciiString1.
	self deny: asciiUtf8String2 < asciiString2.

	self assert: asciiUtf8String0 <= asciiString0.
	self assert: asciiUtf8String0 <= asciiString1.
	self assert: asciiUtf8String0 <= asciiString2.
	self deny: asciiUtf8String1 <= asciiString0.
	self assert: asciiUtf8String1 <= asciiString1.
	self assert: asciiUtf8String1 <= asciiString2.
	self deny: asciiUtf8String2 <= asciiString0.
	self deny: asciiUtf8String2 <= asciiString1.
	self assert: asciiUtf8String2 <= asciiString2.

	self deny: asciiUtf8String0 > asciiString0.
	self deny: asciiUtf8String0 > asciiString1.
	self deny: asciiUtf8String0 > asciiString2.
	self assert: asciiUtf8String1 > asciiString0.
	self deny: asciiUtf8String1 > asciiString1.
	self deny: asciiUtf8String1 > asciiString2.
	self assert: asciiUtf8String2 > asciiString0.
	self assert: asciiUtf8String2 > asciiString1.
	self deny: asciiUtf8String2 > asciiString2.

	self assert: asciiUtf8String0 >= asciiString0.
	self deny: asciiUtf8String0 >= asciiString1.
	self deny: asciiUtf8String0 >= asciiString2.
	self assert: asciiUtf8String1 >= asciiString0.
	self assert: asciiUtf8String1 >= asciiString1.
	self deny: asciiUtf8String1 >= asciiString2.
	self assert: asciiUtf8String2 >= asciiString0.
	self assert: asciiUtf8String2 >= asciiString1.
	self assert: asciiUtf8String2 >= asciiString2.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:43:10 pm (UTC)'!
test08d
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiUtf8String0 < asciiUtf8String0.
	self assert: asciiUtf8String0 < asciiUtf8String1.
	self assert: asciiUtf8String0 < asciiUtf8String2.
	self deny: asciiUtf8String1 < asciiUtf8String0.
	self deny: asciiUtf8String1 < asciiUtf8String1.
	self assert: asciiUtf8String1 < asciiUtf8String2.
	self deny: asciiUtf8String2 < asciiUtf8String0.
	self deny: asciiUtf8String2 < asciiUtf8String1.
	self deny: asciiUtf8String2 < asciiUtf8String2.

	self assert: asciiUtf8String0 <= asciiUtf8String0.
	self assert: asciiUtf8String0 <= asciiUtf8String1.
	self assert: asciiUtf8String0 <= asciiUtf8String2.
	self deny: asciiUtf8String1 <= asciiUtf8String0.
	self assert: asciiUtf8String1 <= asciiUtf8String1.
	self assert: asciiUtf8String1 <= asciiUtf8String2.
	self deny: asciiUtf8String2 <= asciiUtf8String0.
	self deny: asciiUtf8String2 <= asciiUtf8String1.
	self assert: asciiUtf8String2 <= asciiUtf8String2.

	self deny: asciiUtf8String0 > asciiUtf8String0.
	self deny: asciiUtf8String0 > asciiUtf8String1.
	self deny: asciiUtf8String0 > asciiUtf8String2.
	self assert: asciiUtf8String1 > asciiUtf8String0.
	self deny: asciiUtf8String1 > asciiUtf8String1.
	self deny: asciiUtf8String1 > asciiUtf8String2.
	self assert: asciiUtf8String2 > asciiUtf8String0.
	self assert: asciiUtf8String2 > asciiUtf8String1.
	self deny: asciiUtf8String2 > asciiUtf8String2.

	self assert: asciiUtf8String0 >= asciiUtf8String0.
	self deny: asciiUtf8String0 >= asciiUtf8String1.
	self deny: asciiUtf8String0 >= asciiUtf8String2.
	self assert: asciiUtf8String1 >= asciiUtf8String0.
	self assert: asciiUtf8String1 >= asciiUtf8String1.
	self deny: asciiUtf8String1 >= asciiUtf8String2.
	self assert: asciiUtf8String2 >= asciiUtf8String0.
	self assert: asciiUtf8String2 >= asciiUtf8String1.
	self assert: asciiUtf8String2 >= asciiUtf8String2.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:43:26 pm (UTC)'!
test09

	self assert: 'Hello ñandú' asByteString = 'Hello ñandú' asByteString.
	self deny: 'Hello ñandú' asByteString = 'hello ñandú' asByteString.
	self assert: ('Hello ñandú' asByteString sameAs: 'hello ñandú' asByteString).

	self assert: 'Hello ñandú' asUnicodeString = 'Hello ñandú' asUnicodeString.
	self deny: 'Hello ñandú' asUnicodeString = 'hello ñandú' asUnicodeString.
	self assert: ('Hello ñandú' asUnicodeString sameAs: 'hello ñandú' asUnicodeString).

	self assert: 'Hello ñandú' asUnicodeString bytes = 'Hello ñandú' asUnicodeString bytes.
	self deny: 'Hello ñandú' asUnicodeString bytes = 'hello ñandú' asUnicodeString bytes.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:43:50 pm (UTC)'!
test10

	self assert: ('ñandúX' asByteString findString: 'X' asByteString startingAt: 1) = 6.
	self assert: ('ñandúX' asUnicodeString findString: 'X' asByteString startingAt: 1) = 6.
	
	self assert: ('ñandúX' asByteString findString: 'X' startingAt: 7 caseSensitive: false) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'X' startingAt: 7 caseSensitive: false) = 0.
	self assert: ('ñandúX' asByteString findString: 'X' startingAt: 7 caseSensitive: true) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'X' startingAt: 7 caseSensitive: true) = 0.
	self assert: ('ñandúX' asByteString findString: 'x' startingAt: 1 caseSensitive: true) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'x' startingAt: 1 caseSensitive: true) = 0.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:43:59 pm (UTC)'!
test11
	self assert: ('ñandúes' asByteString indexOfSubCollection: 'úe' startingAt: 1 ifAbsent: []) = 5.
	self assert: ('ñandúes' asUnicodeString indexOfSubCollection: 'úe' startingAt: 1 ifAbsent: []) = 5.
	self assert: ('ñandúes' asByteString indexOfSubCollection: 'úe' startingAt: 6 ifAbsent: []) = nil.
	self assert: ('ñandúes' asUnicodeString indexOfSubCollection: 'úe' startingAt: 6 ifAbsent: []) = nil.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:44:14 pm (UTC)'!
test12
	self deny: ('ñandúes' asByteString is: 'úe' substringAt: 4).
	self assert: ('ñandúes' asByteString is: 'úe' substringAt: 5).
	self deny: ('ñandúes' asByteString is: 'úe' substringAt: 6).
	self deny: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 4).
	self assert: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 5).
	self deny: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 6).
	self deny: ('ñandúes' asUnicodeString is: 'úe' substringAt: 4).
	self assert: ('ñandúes'asUnicodeString  is: 'úe' substringAt: 5).
	self deny: ('ñandúes' asUnicodeString is: 'úe' substringAt: 6).
	self deny: ('ñandúes' asUnicodeString is: 'úe' asUnicodeString substringAt: 4).
	self assert: ('ñandúes'asUnicodeString  is: 'úe' asUnicodeString substringAt: 5).
	self deny: ('ñandúes' asUnicodeString is: 'úe' asUnicodeString substringAt: 6).! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 20/Oct/2025, 8:40:37 pm (UTC)'!
testAsUtf8
	"
	 UnicodeStringsTest new testAsUtf8
	"
	| subject |
	subject := 'A¢€'.
	self assert: subject asUtf8Bytes hex = '41C2A2E282AC'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.
	subject := '¿Ñandú, agüita?'.
	self assert: subject asUtf8Bytes hex = 'C2BFC391616E64C3BA2C206167C3BC6974613F'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.
	self assert: subject asByteString asUtf8Bytes = subject asUtf8Bytes.
	self assert: subject asByteString utf8BytesSize = subject asUtf8Bytes size.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 23/Feb/2023, 6:00:34 pm (UTC)'!
testAtPut
	| a b c |
	a _ 'Agüita'.

	b _ a copy.
	self assert: a = b.
	self assert: a hash = b hash.

	c _ b at: 1 put: $Ñ.
	self assert: c = $Ñ.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: (b at: 1) = $Ñ.
	self assert: b = 'Ñgüita'.

	b _ a copy.
	c _ b at: 3 put: $u.
	self assert: c = $u.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: (b at: 3) = $u.
	self assert: b = 'Aguita'.

	b _ a copy.
	c _ b at: b size put: $á.
	self assert: c = $á.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: b last = $á.
	self assert: b = 'Agüitá'.

	b _ a copy.
	c _ b at: b size put: $π.
	self assert: c = $π.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: b last = $π.
	self assert: b = 'Agüitπ'.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2024, 2:58:45 pm (UTC)'!
testEmpty

	| s1 us1 us2 |
	s1 := String new.
	us1 := UnicodeString new.
	us2 := UnicodeString fromUtf8Bytes:#[].

	self assert: s1 hash = us1 hash.
	self assert: s1 = us1.
	self assert: us1 = s1.

	self assert: us2 hash = us1 hash.
	self assert: us2 = us1.
	self assert: us1 = us2.! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 20/Oct/2025, 9:02:21 pm (UTC)'!
testUtf8InvalidSequences
	"
	 UnicodeStringsTest new testUtf8InvalidSequences
	"
	
	self deny: (Character isValidUtf8: #[16rE2 16r28 16rA1]).
	self deny: (Character isValidUtf8: #[16rF8]).
	self deny: (Character isValidUtf8: #[16r92]).
	self deny: (Character isValidUtf8: #[16rF5]).
	self deny: (Character isValidUtf8: #[16rF0 16r90 16r8D]).

	"Cuis correctly allows overlong sequences...
	(UnicodeString fromUtf8Bytes: #[16rC0  16r80]) first asString asUnicodeString bytes"
	"self deny: (Character isValidUtf8: #[16rC0  16r80])."! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 20/Oct/2025, 8:53:39 pm (UTC)'!
testUtf8ValidSequences
	"
	 UnicodeStringsTest new testUtf8ValidSequences
	"
	self assert: (Character isValidUtf8: 'Hello world' asUnicodeString bytes).
	self assert: (Character isValidUtf8: '€' bytes).
	self assert: (Character isValidUtf8: '🐱' bytes).
	self assert: (Character isValidUtf8: '🐱🐱🐱€🐱 ¡Tomá agüita gato!!' bytes).
! !

!UnicodeStringsTest methodsFor: 'testing' stamp: 'jmv 14/Apr/2023, 2:21:49 pm (UTC)'!
testfromBytesStream

	| s large |
	s := 'αℤ⇉⊕§☃§'.
	self assert: s = (UnicodeString fromBytesStream: s asUtf8Bytes readStream).
	large := UnicodeString streamContents: [ :strm |
		1000 timesRepeat: [ strm nextPutAll: s ]].
	self assert: large = (UnicodeString fromBytesStream: large asUtf8Bytes readStream).! !

!UnicodeSymbolsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:44:28 pm (UTC)'!
test01AsciiSymbolFirst
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ 'Stuff' asByteString, Random next mantissaPart printString.
	symbol _ str asSymbol.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.! !

!UnicodeSymbolsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:41:56 pm (UTC)'!
test02LatinSymbolFirst
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ '¡Tomá agüita, Ñandú!!' asByteString, Random next mantissaPart printString.
	symbol _ str asSymbol.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.! !

!UnicodeSymbolsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:45:19 pm (UTC)'!
test03AsciiUtf8SymbolFirst
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ 'Stuff' asByteString, Random next mantissaPart printString.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.
	symbol _ str asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.! !

!UnicodeSymbolsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:42:06 pm (UTC)'!
test04LatinUtf8SymbolFirst
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ '¡Tomá agüita, Ñandú!!' asByteString, Random next mantissaPart printString.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.
	symbol _ str asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.! !

!UnicodeSymbolsTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:33:11 pm (UTC)'!
testSerialization
	self assert: (Object unStream: (ReferenceStream streamedRepresentationOf: 'hello' asUnicodeString asSymbol)) = 'hello' asUnicodeString asSymbol! !

!UnicodeTest methodsFor: 'set up' stamp: 'jmv 20/Mar/2024, 7:44:56 pm (UTC)'!
setUp
	"
	self new setUp
	"
	super setUp.
	bytesOfExample1 := #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
						16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3].

	"see UnicodeNotes.md"

	"write the bytes of an UFT8 encoded string in binary mode to a file"
	self class fileName asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: bytesOfExample1 ]! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 4/Oct/2015, 12:03:00 am (UTC)'!
test1ReadBinary
	
	"see UnicodeNotes.md"
     "
	self new setUp test1ReadBinary
	"
     | content |
	content := self class fileName asFileEntry binaryContents.
	self assert: content = bytesOfExample1! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 4/Oct/2015, 12:04:00 am (UTC)'!
test2ReadWithOutBinary
	
	"see UnicodeNotes.md"
     "
	self new setUp test2ReadWithOutBinary
	"
     | content |
	content := self class fileName asFileEntry textContents.
	self deny: content = bytesOfExample1! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 9/Jan/2024, 4:51:36 pm (UTC)'!
test3ReadUtf8
	
	"see UnicodeNotes.md"
     "
	self new setUp test3ReadUtf8
	"
	| content byteArray |
	byteArray := self class fileName asFileEntry binaryContents.
	content := UnicodeString fromUtf8Bytes: byteArray.
	self assert: content = 'abc àè€ αβγ'.! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 9/Jan/2024, 4:51:52 pm (UTC)'!
test4BackConversion
	"see UnicodeNotes.md"
	"
	self new setUp test4BackConversion
	"
	| contentInternalString contentByteArray |
	contentInternalString := UnicodeString fromUtf8Bytes: self class fileName asFileEntry binaryContents.
	contentByteArray := self class fileName asFileEntry binaryContents.
	self assert: contentByteArray = contentInternalString asUtf8Bytes.! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 9/Jan/2024, 4:52:04 pm (UTC)'!
test5ReadWriteUtf8
	"see UnicodeNotes.md"
	"
	self new setUp test5ReadWriteUtf8
	"
	| content byteArray byteArray2 |
	byteArray := self class fileName asFileEntry binaryContents.
	content := UnicodeString fromUtf8Bytes: byteArray.
	self class fileName2 asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: content asUtf8Bytes ].
	"compare the two versions: what is in file 'fileName' with what is in file 'fileName2'"
	byteArray := self class fileName asFileEntry binaryContents.
	byteArray2 := self class fileName2 asFileEntry binaryContents.
	self assert: byteArray = byteArray2.! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 2:59:54 pm (UTC)'!
testTextConcatenation
	"
	UnicodeTest new testTextConcatenation
	"

	| s1 s2 u1 u2 |
	s1 := 'Hello' asByteString.
	s2 := ' world' asByteString.
	u1 := s1 asUnicodeString.
	u2 := s2 asUnicodeString.

	self assert: s1, u2 equals: s1, s2.
	self assert: u1, s2 equals: s1, s2.
	self assert: u1, u2 equals: s1, s2.

	self assert: s1, u2 bold equals: s1, s2 bold.
	self assert: u1, s2 bold equals: s1, s2 bold.
	self assert: u1, u2 bold equals: s1, s2 bold.

	self assert: s1 bold, u2 equals: s1 bold, s2.
	self assert: u1 bold, s2 equals: s1 bold, s2.
	self assert: u1 bold, u2 equals: s1 bold, s2.

	self assert: s1, u2 bold equals: s1, s2 bold.
	self assert: u1, s2 bold equals: s1, s2 bold.
	self assert: u1, u2 bold equals: s1, s2 bold.! !

!UnicodeTest methodsFor: 'testing utf16' stamp: 'jmv 15/Nov/2023, 6:13:09 pm (UTC)'!
testUtf16BytesFromUtf8
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf16Bytes utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: false includeBOM: false.
	self assert: utf16Bytes = #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: true includeBOM: false.
	self assert: utf16Bytes = #[16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: false includeBOM: true.
	self assert: utf16Bytes = #[16rFF 16rFE 16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: true includeBOM: true.
	self assert: utf16Bytes = #[16rFE 16rFF 16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62].! !

!UnicodeTest methodsFor: 'testing utf16' stamp: 'jmv 5/Jul/2024, 8:12:49 pm (UTC)'!
testUtf16WordsFromUtf8
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf16Words utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].

	utf16Words := UnicodeString utf16WordsFromUtf8: utf8 includeBOM: true.
	self assert: utf16Words asArray = {16rFEFF. 16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62}.

	utf16Words := UnicodeString utf16WordsFromUtf8: utf8 includeBOM: false.
	self assert: utf16Words asArray = {16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62}.! !

!UnicodeTest methodsFor: 'testing utf16' stamp: 'jmv 23/Sep/2024, 2:46:13 pm (UTC)'!
testUtf8FromUtf16Bytes
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16rFF 16rFE 16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16rFE 16rFF 16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF] bigEndianHint: false)
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62] bigEndianHint: true)
		equals: utf8.

	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[])
		equals: #[].! !

!UnicodeTest methodsFor: 'testing utf16' stamp: 'jmv 23/Sep/2024, 2:46:30 pm (UTC)'!
testUtf8FromUtf16Words
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].
	
	self
		assert: (UnicodeString utf8FromUtf16Words: {16rFEFF. 16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62})
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Words: {16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62})
		equals: utf8.

	self
		assert: (UnicodeString utf8FromUtf16Words: {})
		equals: #[].! !

!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 12/Feb/2013, 11:25:00 pm (UTC)'!
fileName
	^'UTF8abc-test.txt'! !

!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 13/Feb/2013, 1:35:00 am (UTC)'!
fileName2
	^'UTF8abc-test2.txt'! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:54 pm (UTC)'!
primTestAdd: aNumber
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 6:01:17 pm (UTC)'!
primTestArcTan

	<primitive: 57>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:58 pm (UTC)'!
primTestDivideBy: aNumber
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:44 pm (UTC)'!
primTestEqual: aNumber
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 4:42:41 pm (UTC)'!
primTestExp
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 59>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 5:42:23 pm (UTC)'!
primTestExponent
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 53>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:41 pm (UTC)'!
primTestGreater: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:38 pm (UTC)'!
primTestGreaterEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:35 pm (UTC)'!
primTestLess: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:32 pm (UTC)'!
primTestLessEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 4:42:00 pm (UTC)'!
primTestLn
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 58>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:21:03 pm (UTC)'!
primTestMultiplyBy: aNumber
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:29 pm (UTC)'!
primTestNotEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^#fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 5:55:50 pm (UTC)'!
primTestSin
	<primitive: 56>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 28/Feb/2019, 7:38:43 pm (UTC)'!
primTestSqrt
	<primitive: 55>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:21:01 pm (UTC)'!
primTestSubtract: aNumber
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ #fail! !

!BoxedFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 11/Mar/2019, 9:05:25 pm (UTC)'!
primTestTruncated
	<primitive: 51>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:13 pm (UTC)'!
primTestAdd: aNumber
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 6:01:27 pm (UTC)'!
primTestArcTan

	<primitive: 557>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:25 pm (UTC)'!
primTestDivideBy: aNumber
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:38:02 pm (UTC)'!
primTestEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 4:42:31 pm (UTC)'!
primTestExp
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 559>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 5:42:28 pm (UTC)'!
primTestExponent
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 553>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:38:00 pm (UTC)'!
primTestGreater: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:57 pm (UTC)'!
primTestGreaterEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:54 pm (UTC)'!
primTestLess: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:52 pm (UTC)'!
primTestLessEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 4:42:10 pm (UTC)'!
primTestLn
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 558>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:40 pm (UTC)'!
primTestMultiplyBy: aNumber
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:37:49 pm (UTC)'!
primTestNotEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^#fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 14/Mar/2019, 5:55:56 pm (UTC)'!
primTestSin
	<primitive: 556>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 28/Feb/2019, 7:38:37 pm (UTC)'!
primTestSqrt
	<primitive: 555>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 25/Feb/2019, 9:20:31 pm (UTC)'!
primTestSubtract: aNumber
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ #fail! !

!SmallFloat64 methodsFor: '*KernelTests-Numbers' stamp: 'jmv 11/Mar/2019, 9:05:16 pm (UTC)'!
primTestTruncated
	<primitive: 551>
	^ #fail! !
