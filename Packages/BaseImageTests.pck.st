'From Cuis 5.0 [latest update: #4519] on 6 January 2021 at 12:01:55 pm'!
'Description ReturnNodeTest tests'!
!provides: 'BaseImageTests' 1 254!
!requires: '__Refactoring-TestData__' 1 0 nil!
SystemOrganization addCategory: 'BaseImageTests-Collections'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Classes'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Numbers'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Chronology'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Methods'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Processes'!
SystemOrganization addCategory: 'BaseImageTests-Kernel'!
SystemOrganization addCategory: 'BaseImageTests-Collections-Arrayed'!
SystemOrganization addCategory: 'BaseImageTests-Exceptions'!
SystemOrganization addCategory: 'BaseImageTests-Compiler'!
SystemOrganization addCategory: 'BaseImageTests-System-FileMan'!
SystemOrganization addCategory: 'BaseImageTests-System-Files'!
SystemOrganization addCategory: 'BaseImageTests-System'!
SystemOrganization addCategory: 'BaseImageTests-System-Object Storage'!
SystemOrganization addCategory: 'BaseImageTests-Graphics Primitives'!
SystemOrganization addCategory: 'BaseImageTests-Graphics'!
SystemOrganization addCategory: 'BaseImageTests-Morphic-Kernel'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Autocompletion'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Taskbar'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Testing'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Code Differ'!
SystemOrganization addCategory: 'BaseImageTests-Unicode'!
SystemOrganization addCategory: 'BaseImageTests-Bugs'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Refactoring'!
SystemOrganization addCategory: 'BaseImageTests-Collections-Streams'!
SystemOrganization addCategory: 'BaseImageTests-System-Support'!
SystemOrganization addCategory: 'BaseImageTests-Graphics-Primitives'!
SystemOrganization addCategory: 'BaseImageTests-Graphics-Display Objects'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Objects'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Browser'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Changes'!
SystemOrganization addCategory: 'BaseImageTests-Kernel-Text'!
SystemOrganization addCategory: 'BaseImageTests-Tools-Syntax Highlighting'!


!classDefinition: #MyResumableTestError category: 'BaseImageTests-Exceptions'!
Error subclass: #MyResumableTestError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'MyResumableTestError class' category: 'BaseImageTests-Exceptions'!
MyResumableTestError class
	instanceVariableNames: ''!

!classDefinition: #MyTestError category: 'BaseImageTests-Exceptions'!
Error subclass: #MyTestError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'MyTestError class' category: 'BaseImageTests-Exceptions'!
MyTestError class
	instanceVariableNames: ''!

!classDefinition: #MyTestNotification category: 'BaseImageTests-Exceptions'!
Notification subclass: #MyTestNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'MyTestNotification class' category: 'BaseImageTests-Exceptions'!
MyTestNotification class
	instanceVariableNames: ''!

!classDefinition: #BinarySearchTest category: 'BaseImageTests-Collections'!
TestCase subclass: #BinarySearchTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'BinarySearchTest class' category: 'BaseImageTests-Collections'!
BinarySearchTest class
	instanceVariableNames: ''!

!classDefinition: #CollectTest category: 'BaseImageTests-Collections'!
TestCase subclass: #CollectTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'CollectTest class' category: 'BaseImageTests-Collections'!
CollectTest class
	instanceVariableNames: ''!

!classDefinition: #CollectionTest category: 'BaseImageTests-Collections'!
TestCase subclass: #CollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'CollectionTest class' category: 'BaseImageTests-Collections'!
CollectionTest class
	instanceVariableNames: ''!

!classDefinition: #DictionaryTest category: 'BaseImageTests-Collections'!
TestCase subclass: #DictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'DictionaryTest class' category: 'BaseImageTests-Collections'!
DictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #IntervalTest category: 'BaseImageTests-Collections'!
TestCase subclass: #IntervalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'IntervalTest class' category: 'BaseImageTests-Collections'!
IntervalTest class
	instanceVariableNames: ''!

!classDefinition: #OrderedCollectionTest category: 'BaseImageTests-Collections'!
TestCase subclass: #OrderedCollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'OrderedCollectionTest class' category: 'BaseImageTests-Collections'!
OrderedCollectionTest class
	instanceVariableNames: ''!

!classDefinition: #OrderedDictionaryTest category: 'BaseImageTests-Collections'!
TestCase subclass: #OrderedDictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'OrderedDictionaryTest class' category: 'BaseImageTests-Collections'!
OrderedDictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #SequenceableCollectionTest category: 'BaseImageTests-Collections'!
TestCase subclass: #SequenceableCollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'SequenceableCollectionTest class' category: 'BaseImageTests-Collections'!
SequenceableCollectionTest class
	instanceVariableNames: ''!

!classDefinition: #TextTest category: 'BaseImageTests-Collections'!
TestCase subclass: #TextTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'TextTest class' category: 'BaseImageTests-Collections'!
TextTest class
	instanceVariableNames: ''!

!classDefinition: #WeakIdentitySetTest category: 'BaseImageTests-Collections'!
TestCase subclass: #WeakIdentitySetTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections'!
!classDefinition: 'WeakIdentitySetTest class' category: 'BaseImageTests-Collections'!
WeakIdentitySetTest class
	instanceVariableNames: ''!

!classDefinition: #BehaviorTest category: 'BaseImageTests-Kernel-Classes'!
TestCase subclass: #BehaviorTest
	instanceVariableNames: 'unreferenced referenced'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Classes'!
!classDefinition: 'BehaviorTest class' category: 'BaseImageTests-Kernel-Classes'!
BehaviorTest class
	instanceVariableNames: ''!

!classDefinition: #BehaviorTestSubclass category: 'BaseImageTests-Kernel-Classes'!
BehaviorTest subclass: #BehaviorTestSubclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Classes'!
!classDefinition: 'BehaviorTestSubclass class' category: 'BaseImageTests-Kernel-Classes'!
BehaviorTestSubclass class
	instanceVariableNames: ''!

!classDefinition: #CategorizerTest category: 'BaseImageTests-Kernel-Classes'!
TestCase subclass: #CategorizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Classes'!
!classDefinition: 'CategorizerTest class' category: 'BaseImageTests-Kernel-Classes'!
CategorizerTest class
	instanceVariableNames: ''!

!classDefinition: #ClassOrganizerTest category: 'BaseImageTests-Kernel-Classes'!
CategorizerTest subclass: #ClassOrganizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Classes'!
!classDefinition: 'ClassOrganizerTest class' category: 'BaseImageTests-Kernel-Classes'!
ClassOrganizerTest class
	instanceVariableNames: ''!

!classDefinition: #ClassTest category: 'BaseImageTests-Kernel-Classes'!
TestCase subclass: #ClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Classes'!
!classDefinition: 'ClassTest class' category: 'BaseImageTests-Kernel-Classes'!
ClassTest class
	instanceVariableNames: ''!

!classDefinition: #AbstractNumberTest category: 'BaseImageTests-Kernel-Numbers'!
TestCase subclass: #AbstractNumberTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'AbstractNumberTest class' category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest class
	instanceVariableNames: ''!

!classDefinition: #FloatTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #FloatTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'FloatTest class' category: 'BaseImageTests-Kernel-Numbers'!
FloatTest class
	instanceVariableNames: ''!

!classDefinition: #FractionTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #FractionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'FractionTest class' category: 'BaseImageTests-Kernel-Numbers'!
FractionTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerDigitLogicTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #IntegerDigitLogicTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'IntegerDigitLogicTest class' category: 'BaseImageTests-Kernel-Numbers'!
IntegerDigitLogicTest class
	instanceVariableNames: ''!

!classDefinition: #IntegerTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #IntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'IntegerTest class' category: 'BaseImageTests-Kernel-Numbers'!
IntegerTest class
	instanceVariableNames: ''!

!classDefinition: #LargeNegativeIntegerTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #LargeNegativeIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'LargeNegativeIntegerTest class' category: 'BaseImageTests-Kernel-Numbers'!
LargeNegativeIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #LargePositiveIntegerTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #LargePositiveIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'LargePositiveIntegerTest class' category: 'BaseImageTests-Kernel-Numbers'!
LargePositiveIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #NumberTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #NumberTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'NumberTest class' category: 'BaseImageTests-Kernel-Numbers'!
NumberTest class
	instanceVariableNames: ''!

!classDefinition: #SmallIntegerTest category: 'BaseImageTests-Kernel-Numbers'!
AbstractNumberTest subclass: #SmallIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'SmallIntegerTest class' category: 'BaseImageTests-Kernel-Numbers'!
SmallIntegerTest class
	instanceVariableNames: ''!

!classDefinition: #RandomTest category: 'BaseImageTests-Kernel-Numbers'!
TestCase subclass: #RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'RandomTest class' category: 'BaseImageTests-Kernel-Numbers'!
RandomTest class
	instanceVariableNames: ''!

!classDefinition: #LaggedFibonacciRandomTest category: 'BaseImageTests-Kernel-Numbers'!
RandomTest subclass: #LaggedFibonacciRandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'LaggedFibonacciRandomTest class' category: 'BaseImageTests-Kernel-Numbers'!
LaggedFibonacciRandomTest class
	instanceVariableNames: ''!

!classDefinition: #ParkMiller88RandomTest category: 'BaseImageTests-Kernel-Numbers'!
RandomTest subclass: #ParkMiller88RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'ParkMiller88RandomTest class' category: 'BaseImageTests-Kernel-Numbers'!
ParkMiller88RandomTest class
	instanceVariableNames: ''!

!classDefinition: #ParkMiller93RandomTest category: 'BaseImageTests-Kernel-Numbers'!
RandomTest subclass: #ParkMiller93RandomTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Numbers'!
!classDefinition: 'ParkMiller93RandomTest class' category: 'BaseImageTests-Kernel-Numbers'!
ParkMiller93RandomTest class
	instanceVariableNames: ''!

!classDefinition: #DateAndTimeTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #DateAndTimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'DateAndTimeTest class' category: 'BaseImageTests-Kernel-Chronology'!
DateAndTimeTest class
	instanceVariableNames: ''!

!classDefinition: #DateTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #DateTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'DateTest class' category: 'BaseImageTests-Kernel-Chronology'!
DateTest class
	instanceVariableNames: ''!

!classDefinition: #DurationTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #DurationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'DurationTest class' category: 'BaseImageTests-Kernel-Chronology'!
DurationTest class
	instanceVariableNames: ''!

!classDefinition: #MonthTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #MonthTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'MonthTest class' category: 'BaseImageTests-Kernel-Chronology'!
MonthTest class
	instanceVariableNames: ''!

!classDefinition: #TimeTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #TimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'TimeTest class' category: 'BaseImageTests-Kernel-Chronology'!
TimeTest class
	instanceVariableNames: ''!

!classDefinition: #TimespanTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #TimespanTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'TimespanTest class' category: 'BaseImageTests-Kernel-Chronology'!
TimespanTest class
	instanceVariableNames: ''!

!classDefinition: #WeekTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #WeekTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'WeekTest class' category: 'BaseImageTests-Kernel-Chronology'!
WeekTest class
	instanceVariableNames: ''!

!classDefinition: #YearTest category: 'BaseImageTests-Kernel-Chronology'!
TestCase subclass: #YearTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Chronology'!
!classDefinition: 'YearTest class' category: 'BaseImageTests-Kernel-Chronology'!
YearTest class
	instanceVariableNames: ''!

!classDefinition: #CompiledMethodTest category: 'BaseImageTests-Kernel-Methods'!
TestCase subclass: #CompiledMethodTest
	instanceVariableNames: 'iv1 iv2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Methods'!
!classDefinition: 'CompiledMethodTest class' category: 'BaseImageTests-Kernel-Methods'!
CompiledMethodTest class
	instanceVariableNames: ''!

!classDefinition: #DelayTest category: 'BaseImageTests-Kernel-Processes'!
TestCase subclass: #DelayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Processes'!
!classDefinition: 'DelayTest class' category: 'BaseImageTests-Kernel-Processes'!
DelayTest class
	instanceVariableNames: ''!

!classDefinition: #MonitorTest category: 'BaseImageTests-Kernel-Processes'!
TestCase subclass: #MonitorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Processes'!
!classDefinition: 'MonitorTest class' category: 'BaseImageTests-Kernel-Processes'!
MonitorTest class
	instanceVariableNames: ''!

!classDefinition: #ProcessTest category: 'BaseImageTests-Kernel-Processes'!
TestCase subclass: #ProcessTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Processes'!
!classDefinition: 'ProcessTest class' category: 'BaseImageTests-Kernel-Processes'!
ProcessTest class
	instanceVariableNames: ''!

!classDefinition: #SemaphoreTest category: 'BaseImageTests-Kernel-Processes'!
TestCase subclass: #SemaphoreTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Processes'!
!classDefinition: 'SemaphoreTest class' category: 'BaseImageTests-Kernel-Processes'!
SemaphoreTest class
	instanceVariableNames: ''!

!classDefinition: #BecomeTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #BecomeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'BecomeTest class' category: 'BaseImageTests-Kernel'!
BecomeTest class
	instanceVariableNames: ''!

!classDefinition: #BooleanTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #BooleanTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'BooleanTest class' category: 'BaseImageTests-Kernel'!
BooleanTest class
	instanceVariableNames: ''!

!classDefinition: #CharacterSetTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #CharacterSetTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'CharacterSetTest class' category: 'BaseImageTests-Kernel'!
CharacterSetTest class
	instanceVariableNames: ''!

!classDefinition: #CharacterTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #CharacterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'CharacterTest class' category: 'BaseImageTests-Kernel'!
CharacterTest class
	instanceVariableNames: ''!

!classDefinition: #IfNotNilTests category: 'BaseImageTests-Kernel'!
TestCase subclass: #IfNotNilTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'IfNotNilTests class' category: 'BaseImageTests-Kernel'!
IfNotNilTests class
	instanceVariableNames: ''!

!classDefinition: #ProcessorTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #ProcessorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'ProcessorTest class' category: 'BaseImageTests-Kernel'!
ProcessorTest class
	instanceVariableNames: ''!

!classDefinition: #SpecialSelectorsTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #SpecialSelectorsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'SpecialSelectorsTest class' category: 'BaseImageTests-Kernel'!
SpecialSelectorsTest class
	instanceVariableNames: ''!

!classDefinition: #StrikeFontTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #StrikeFontTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'StrikeFontTest class' category: 'BaseImageTests-Kernel'!
StrikeFontTest class
	instanceVariableNames: ''!

!classDefinition: #StringTest category: 'BaseImageTests-Kernel'!
TestCase subclass: #StringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'StringTest class' category: 'BaseImageTests-Kernel'!
StringTest class
	instanceVariableNames: ''!

!classDefinition: #ArrayTest category: 'BaseImageTests-Collections-Arrayed'!
TestCase subclass: #ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections-Arrayed'!
!classDefinition: 'ArrayTest class' category: 'BaseImageTests-Collections-Arrayed'!
ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #Float32ArrayTest category: 'BaseImageTests-Collections-Arrayed'!
TestCase subclass: #Float32ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections-Arrayed'!
!classDefinition: 'Float32ArrayTest class' category: 'BaseImageTests-Collections-Arrayed'!
Float32ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #Float64ArrayTest category: 'BaseImageTests-Collections-Arrayed'!
TestCase subclass: #Float64ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections-Arrayed'!
!classDefinition: 'Float64ArrayTest class' category: 'BaseImageTests-Collections-Arrayed'!
Float64ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ExceptionHandlingConditionTest category: 'BaseImageTests-Exceptions'!
TestCase subclass: #ExceptionHandlingConditionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'ExceptionHandlingConditionTest class' category: 'BaseImageTests-Exceptions'!
ExceptionHandlingConditionTest class
	instanceVariableNames: ''!

!classDefinition: #ExceptionTests category: 'BaseImageTests-Exceptions'!
TestCase subclass: #ExceptionTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'ExceptionTests class' category: 'BaseImageTests-Exceptions'!
ExceptionTests class
	instanceVariableNames: ''!

!classDefinition: #ProcessTerminateBug category: 'BaseImageTests-Exceptions'!
TestCase subclass: #ProcessTerminateBug
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'ProcessTerminateBug class' category: 'BaseImageTests-Exceptions'!
ProcessTerminateBug class
	instanceVariableNames: ''!

!classDefinition: #ArrayLiteralTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ArrayLiteralTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ArrayLiteralTest class' category: 'BaseImageTests-Compiler'!
ArrayLiteralTest class
	instanceVariableNames: ''!

!classDefinition: #ClassDefinitionNodeAnalyzerTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ClassDefinitionNodeAnalyzerTest
	instanceVariableNames: 'iv1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ClassDefinitionNodeAnalyzerTest class' category: 'BaseImageTests-Compiler'!
ClassDefinitionNodeAnalyzerTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureCompilerTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ClosureCompilerTest
	instanceVariableNames: ''
	classVariableNames: 'CmpRR CogRTLOpcodes Jump MoveCqR Nop'
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ClosureCompilerTest class' category: 'BaseImageTests-Compiler'!
ClosureCompilerTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureSerializationTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ClosureSerializationTest
	instanceVariableNames: 'ivar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ClosureSerializationTest class' category: 'BaseImageTests-Compiler'!
ClosureSerializationTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureTests category: 'BaseImageTests-Compiler'!
TestCase subclass: #ClosureTests
	instanceVariableNames: 'collection ivar'
	classVariableNames: 'ClassVar'
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ClosureTests class' category: 'BaseImageTests-Compiler'!
ClosureTests class
	instanceVariableNames: ''!

!classDefinition: #ContextCompilationTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ContextCompilationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ContextCompilationTest class' category: 'BaseImageTests-Compiler'!
ContextCompilationTest class
	instanceVariableNames: ''!

!classDefinition: #DecompilerTests category: 'BaseImageTests-Compiler'!
TestCase subclass: #DecompilerTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'DecompilerTests class' category: 'BaseImageTests-Compiler'!
DecompilerTests class
	instanceVariableNames: ''!

!classDefinition: #DecompilerTestFailuresCollector category: 'BaseImageTests-Compiler'!
DecompilerTests subclass: #DecompilerTestFailuresCollector
	instanceVariableNames: 'failures'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'DecompilerTestFailuresCollector class' category: 'BaseImageTests-Compiler'!
DecompilerTestFailuresCollector class
	instanceVariableNames: ''!

!classDefinition: #MirrorPrimitiveTests category: 'BaseImageTests-Compiler'!
TestCase subclass: #MirrorPrimitiveTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'MirrorPrimitiveTests class' category: 'BaseImageTests-Compiler'!
MirrorPrimitiveTests class
	instanceVariableNames: ''!

!classDefinition: #ParseNodeEnumeratorTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ParseNodeEnumeratorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ParseNodeEnumeratorTest class' category: 'BaseImageTests-Compiler'!
ParseNodeEnumeratorTest class
	instanceVariableNames: ''!

!classDefinition: #ParserTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ParserTest class' category: 'BaseImageTests-Compiler'!
ParserTest class
	instanceVariableNames: ''!

!classDefinition: #PrettyPrintingTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #PrettyPrintingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'PrettyPrintingTest class' category: 'BaseImageTests-Compiler'!
PrettyPrintingTest class
	instanceVariableNames: ''!

!classDefinition: #ReturnNodeTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ReturnNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ReturnNodeTest class' category: 'BaseImageTests-Compiler'!
ReturnNodeTest class
	instanceVariableNames: ''!

!classDefinition: #ScannerTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #ScannerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'ScannerTest class' category: 'BaseImageTests-Compiler'!
ScannerTest class
	instanceVariableNames: ''!

!classDefinition: #SourceCodeIntervalTest category: 'BaseImageTests-Compiler'!
TestCase subclass: #SourceCodeIntervalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'SourceCodeIntervalTest class' category: 'BaseImageTests-Compiler'!
SourceCodeIntervalTest class
	instanceVariableNames: ''!

!classDefinition: #FileIOAccessorTest category: 'BaseImageTests-System-FileMan'!
TestCase subclass: #FileIOAccessorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-FileMan'!
!classDefinition: 'FileIOAccessorTest class' category: 'BaseImageTests-System-FileMan'!
FileIOAccessorTest class
	instanceVariableNames: ''!

!classDefinition: #FileManTest category: 'BaseImageTests-System-FileMan'!
TestCase subclass: #FileManTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-FileMan'!
!classDefinition: 'FileManTest class' category: 'BaseImageTests-System-FileMan'!
FileManTest class
	instanceVariableNames: ''!

!classDefinition: #CuisSourceFileArrayTest category: 'BaseImageTests-System-Files'!
TestCase subclass: #CuisSourceFileArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-Files'!
!classDefinition: 'CuisSourceFileArrayTest class' category: 'BaseImageTests-System-Files'!
CuisSourceFileArrayTest class
	instanceVariableNames: ''!

!classDefinition: #SystemConsistencyTest category: 'BaseImageTests-System'!
TestCase subclass: #SystemConsistencyTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System'!
!classDefinition: 'SystemConsistencyTest class' category: 'BaseImageTests-System'!
SystemConsistencyTest class
	instanceVariableNames: ''!

!classDefinition: #SystemDictionaryTest category: 'BaseImageTests-System'!
TestCase subclass: #SystemDictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System'!
!classDefinition: 'SystemDictionaryTest class' category: 'BaseImageTests-System'!
SystemDictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #TrieTest category: 'BaseImageTests-System'!
TestCase subclass: #TrieTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System'!
!classDefinition: 'TrieTest class' category: 'BaseImageTests-System'!
TrieTest class
	instanceVariableNames: ''!

!classDefinition: #ReferenceStreamTest category: 'BaseImageTests-System-Object Storage'!
TestCase subclass: #ReferenceStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-Object Storage'!
!classDefinition: 'ReferenceStreamTest class' category: 'BaseImageTests-System-Object Storage'!
ReferenceStreamTest class
	instanceVariableNames: ''!

!classDefinition: #SmartRefStreamTest category: 'BaseImageTests-System-Object Storage'!
TestCase subclass: #SmartRefStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-Object Storage'!
!classDefinition: 'SmartRefStreamTest class' category: 'BaseImageTests-System-Object Storage'!
SmartRefStreamTest class
	instanceVariableNames: ''!

!classDefinition: #BitBltTest category: 'BaseImageTests-Graphics Primitives'!
TestCase subclass: #BitBltTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Graphics Primitives'!
!classDefinition: 'BitBltTest class' category: 'BaseImageTests-Graphics Primitives'!
BitBltTest class
	instanceVariableNames: ''!

!classDefinition: #JpegTest category: 'BaseImageTests-Graphics'!
TestCase subclass: #JpegTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Graphics'!
!classDefinition: 'JpegTest class' category: 'BaseImageTests-Graphics'!
JpegTest class
	instanceVariableNames: ''!

!classDefinition: #AffineTransformationTest category: 'BaseImageTests-Morphic-Kernel'!
TestCase subclass: #AffineTransformationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Morphic-Kernel'!
!classDefinition: 'AffineTransformationTest class' category: 'BaseImageTests-Morphic-Kernel'!
AffineTransformationTest class
	instanceVariableNames: ''!

!classDefinition: #LayoutMorphTest category: 'BaseImageTests-Morphic-Kernel'!
TestCase subclass: #LayoutMorphTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Morphic-Kernel'!
!classDefinition: 'LayoutMorphTest class' category: 'BaseImageTests-Morphic-Kernel'!
LayoutMorphTest class
	instanceVariableNames: ''!

!classDefinition: #MorphicLocationTest category: 'BaseImageTests-Morphic-Kernel'!
TestCase subclass: #MorphicLocationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Morphic-Kernel'!
!classDefinition: 'MorphicLocationTest class' category: 'BaseImageTests-Morphic-Kernel'!
MorphicLocationTest class
	instanceVariableNames: ''!

!classDefinition: #MorphicTranslationTest category: 'BaseImageTests-Morphic-Kernel'!
TestCase subclass: #MorphicTranslationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Morphic-Kernel'!
!classDefinition: 'MorphicTranslationTest class' category: 'BaseImageTests-Morphic-Kernel'!
MorphicTranslationTest class
	instanceVariableNames: ''!

!classDefinition: #WorldTest category: 'BaseImageTests-Morphic-Kernel'!
TestCase subclass: #WorldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Morphic-Kernel'!
!classDefinition: 'WorldTest class' category: 'BaseImageTests-Morphic-Kernel'!
WorldTest class
	instanceVariableNames: ''!

!classDefinition: #SmalltalkCompleterTest category: 'BaseImageTests-Tools-Autocompletion'!
TestCase subclass: #SmalltalkCompleterTest
	instanceVariableNames: 'instanceVariable1 classToBrowse selectorToBrowse'
	classVariableNames: 'ClassVariableForTesting'
	poolDictionaries: 'poolDictionaryForTesting'
	category: 'BaseImageTests-Tools-Autocompletion'!
!classDefinition: 'SmalltalkCompleterTest class' category: 'BaseImageTests-Tools-Autocompletion'!
SmalltalkCompleterTest class
	instanceVariableNames: ''!

!classDefinition: #DynamicTypingSmalltalkCompleterTest category: 'BaseImageTests-Tools-Autocompletion'!
SmalltalkCompleterTest subclass: #DynamicTypingSmalltalkCompleterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Autocompletion'!
!classDefinition: 'DynamicTypingSmalltalkCompleterTest class' category: 'BaseImageTests-Tools-Autocompletion'!
DynamicTypingSmalltalkCompleterTest class
	instanceVariableNames: ''!

!classDefinition: #TaskbarTest category: 'BaseImageTests-Tools-Taskbar'!
TestCase subclass: #TaskbarTest
	instanceVariableNames: 'taskbar needsDelete'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Taskbar'!
!classDefinition: 'TaskbarTest class' category: 'BaseImageTests-Tools-Taskbar'!
TaskbarTest class
	instanceVariableNames: ''!

!classDefinition: #TestCaseTest category: 'BaseImageTests-Tools-Testing'!
TestCase subclass: #TestCaseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Testing'!
!classDefinition: 'TestCaseTest class' category: 'BaseImageTests-Tools-Testing'!
TestCaseTest class
	instanceVariableNames: ''!

!classDefinition: #TestSuiteTest category: 'BaseImageTests-Tools-Testing'!
TestCase subclass: #TestSuiteTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Testing'!
!classDefinition: 'TestSuiteTest class' category: 'BaseImageTests-Tools-Testing'!
TestSuiteTest class
	instanceVariableNames: ''!

!classDefinition: #DifferenceFinderTest category: 'BaseImageTests-Tools-Code Differ'!
TestCase subclass: #DifferenceFinderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Code Differ'!
!classDefinition: 'DifferenceFinderTest class' category: 'BaseImageTests-Tools-Code Differ'!
DifferenceFinderTest class
	instanceVariableNames: ''!

!classDefinition: #UnicodeTest category: 'BaseImageTests-Unicode'!
TestCase subclass: #UnicodeTest
	instanceVariableNames: 'bytesOfExample1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Unicode'!
!classDefinition: 'UnicodeTest class' category: 'BaseImageTests-Unicode'!
UnicodeTest class
	instanceVariableNames: ''!

!classDefinition: #TestValueWithinFix category: 'BaseImageTests-Bugs'!
TestCase subclass: #TestValueWithinFix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Bugs'!
!classDefinition: 'TestValueWithinFix class' category: 'BaseImageTests-Bugs'!
TestValueWithinFix class
	instanceVariableNames: ''!

!classDefinition: #DynamicallyCodeCreationTest category: 'BaseImageTests-Tools-Refactoring'!
TestCase subclass: #DynamicallyCodeCreationTest
	instanceVariableNames: 'classCategories setUpAssertionsPassed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'DynamicallyCodeCreationTest class' category: 'BaseImageTests-Tools-Refactoring'!
DynamicallyCodeCreationTest class
	instanceVariableNames: ''!

!classDefinition: #CompilerTest category: 'BaseImageTests-Compiler'!
DynamicallyCodeCreationTest subclass: #CompilerTest
	instanceVariableNames: 'sourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Compiler'!
!classDefinition: 'CompilerTest class' category: 'BaseImageTests-Compiler'!
CompilerTest class
	instanceVariableNames: ''!

!classDefinition: #RefactoringTest category: 'BaseImageTests-Tools-Refactoring'!
DynamicallyCodeCreationTest subclass: #RefactoringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RefactoringTest class' category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest class
	instanceVariableNames: ''!

!classDefinition: #AddInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #AddInstanceVariableTest
	instanceVariableNames: 'newVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'AddInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
AddInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #AddParameterTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #AddParameterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'AddParameterTest class' category: 'BaseImageTests-Tools-Refactoring'!
AddParameterTest class
	instanceVariableNames: ''!

!classDefinition: #ChangeKeywordsSelectorOrderTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #ChangeKeywordsSelectorOrderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'ChangeKeywordsSelectorOrderTest class' category: 'BaseImageTests-Tools-Refactoring'!
ChangeKeywordsSelectorOrderTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractMethodTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #ExtractMethodTest
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'ExtractMethodTest class' category: 'BaseImageTests-Tools-Refactoring'!
ExtractMethodTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractToTemporaryTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #ExtractToTemporaryTest
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'ExtractToTemporaryTest class' category: 'BaseImageTests-Tools-Refactoring'!
ExtractToTemporaryTest class
	instanceVariableNames: ''!

!classDefinition: #InsertSuperclassTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #InsertSuperclassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'InsertSuperclassTest class' category: 'BaseImageTests-Tools-Refactoring'!
InsertSuperclassTest class
	instanceVariableNames: ''!

!classDefinition: #MoveToInstanceOrClassMethodTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #MoveToInstanceOrClassMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'MoveToInstanceOrClassMethodTest class' category: 'BaseImageTests-Tools-Refactoring'!
MoveToInstanceOrClassMethodTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #PushDownInstanceVariableTest
	instanceVariableNames: 'classToRefactor instanceVariableToPushDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'PushDownInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
PushDownInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownMethodTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #PushDownMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'PushDownMethodTest class' category: 'BaseImageTests-Tools-Refactoring'!
PushDownMethodTest class
	instanceVariableNames: ''!

!classDefinition: #PushUpInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #PushUpInstanceVariableTest
	instanceVariableNames: 'classToRefactor instanceVariableToPushUp classToRefactorSuperClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'PushUpInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
PushUpInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #PushUpMethodTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #PushUpMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'PushUpMethodTest class' category: 'BaseImageTests-Tools-Refactoring'!
PushUpMethodTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveAllUnreferencedInstanceVariablesTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RemoveAllUnreferencedInstanceVariablesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RemoveAllUnreferencedInstanceVariablesTest class' category: 'BaseImageTests-Tools-Refactoring'!
RemoveAllUnreferencedInstanceVariablesTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RemoveInstanceVariableTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RemoveInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
RemoveInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveParameterTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RemoveParameterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RemoveParameterTest class' category: 'BaseImageTests-Tools-Refactoring'!
RemoveParameterTest class
	instanceVariableNames: ''!

!classDefinition: #RenameClassTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RenameClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RenameClassTest class' category: 'BaseImageTests-Tools-Refactoring'!
RenameClassTest class
	instanceVariableNames: ''!

!classDefinition: #RenameGlobalTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RenameGlobalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RenameGlobalTest class' category: 'BaseImageTests-Tools-Refactoring'!
RenameGlobalTest class
	instanceVariableNames: ''!

!classDefinition: #RenameInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RenameInstanceVariableTest
	instanceVariableNames: 'oldVariable newVariable classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RenameInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
RenameInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #RenameSelectorTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RenameSelectorTest
	instanceVariableNames: 'oldSelector newSelector senderOfOldSelector classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RenameSelectorTest class' category: 'BaseImageTests-Tools-Refactoring'!
RenameSelectorTest class
	instanceVariableNames: ''!

!classDefinition: #RenameTemporaryTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #RenameTemporaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RenameTemporaryTest class' category: 'BaseImageTests-Tools-Refactoring'!
RenameTemporaryTest class
	instanceVariableNames: ''!

!classDefinition: #SafelyRemoveClassTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #SafelyRemoveClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'SafelyRemoveClassTest class' category: 'BaseImageTests-Tools-Refactoring'!
SafelyRemoveClassTest class
	instanceVariableNames: ''!

!classDefinition: #TemporaryToInstanceVariableTest category: 'BaseImageTests-Tools-Refactoring'!
RefactoringTest subclass: #TemporaryToInstanceVariableTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'TemporaryToInstanceVariableTest class' category: 'BaseImageTests-Tools-Refactoring'!
TemporaryToInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #ChangesTest category: 'BaseImageTests-Tools-Changes'!
DynamicallyCodeCreationTest subclass: #ChangesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Changes'!
!classDefinition: 'ChangesTest class' category: 'BaseImageTests-Tools-Changes'!
ChangesTest class
	instanceVariableNames: ''!

!classDefinition: #WriteStreamTest category: 'BaseImageTests-Collections-Streams'!
TestCase subclass: #WriteStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Collections-Streams'!
!classDefinition: 'WriteStreamTest class' category: 'BaseImageTests-Collections-Streams'!
WriteStreamTest class
	instanceVariableNames: ''!

!classDefinition: #TranscriptTest category: 'BaseImageTests-System-Support'!
TestCase subclass: #TranscriptTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-Support'!
!classDefinition: 'TranscriptTest class' category: 'BaseImageTests-System-Support'!
TranscriptTest class
	instanceVariableNames: ''!

!classDefinition: #RectangleTest category: 'BaseImageTests-Graphics-Primitives'!
TestCase subclass: #RectangleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Graphics-Primitives'!
!classDefinition: 'RectangleTest class' category: 'BaseImageTests-Graphics-Primitives'!
RectangleTest class
	instanceVariableNames: ''!

!classDefinition: #ColorFormTest category: 'BaseImageTests-Graphics-Display Objects'!
TestCase subclass: #ColorFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Graphics-Display Objects'!
!classDefinition: 'ColorFormTest class' category: 'BaseImageTests-Graphics-Display Objects'!
ColorFormTest class
	instanceVariableNames: ''!

!classDefinition: #GrayFormTest category: 'BaseImageTests-Graphics-Display Objects'!
TestCase subclass: #GrayFormTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Graphics-Display Objects'!
!classDefinition: 'GrayFormTest class' category: 'BaseImageTests-Graphics-Display Objects'!
GrayFormTest class
	instanceVariableNames: ''!

!classDefinition: #ObjectTest category: 'BaseImageTests-Kernel-Objects'!
TestCase subclass: #ObjectTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Objects'!
!classDefinition: 'ObjectTest class' category: 'BaseImageTests-Kernel-Objects'!
ObjectTest class
	instanceVariableNames: ''!

!classDefinition: #WeakMessageSendTest category: 'BaseImageTests-Kernel-Objects'!
TestCase subclass: #WeakMessageSendTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Objects'!
!classDefinition: 'WeakMessageSendTest class' category: 'BaseImageTests-Kernel-Objects'!
WeakMessageSendTest class
	instanceVariableNames: ''!

!classDefinition: #MethodReferenceTest category: 'BaseImageTests-Tools-Browser'!
TestCase subclass: #MethodReferenceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Browser'!
!classDefinition: 'MethodReferenceTest class' category: 'BaseImageTests-Tools-Browser'!
MethodReferenceTest class
	instanceVariableNames: ''!

!classDefinition: #SymbolTest category: 'BaseImageTests-Kernel-Text'!
TestCase subclass: #SymbolTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel-Text'!
!classDefinition: 'SymbolTest class' category: 'BaseImageTests-Kernel-Text'!
SymbolTest class
	instanceVariableNames: ''!

!classDefinition: #SHST80RangeTypeTest category: 'BaseImageTests-Tools-Syntax Highlighting'!
TestCase subclass: #SHST80RangeTypeTest
	instanceVariableNames: ''
	classVariableNames: 'ClassVar1'
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Syntax Highlighting'!
!classDefinition: 'SHST80RangeTypeTest class' category: 'BaseImageTests-Tools-Syntax Highlighting'!
SHST80RangeTypeTest class
	instanceVariableNames: ''!

!classDefinition: #BecomeTestExperiment category: 'BaseImageTests-Kernel'!
Object subclass: #BecomeTestExperiment
	instanceVariableNames: 'a b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Kernel'!
!classDefinition: 'BecomeTestExperiment class' category: 'BaseImageTests-Kernel'!
BecomeTestExperiment class
	instanceVariableNames: ''!

!classDefinition: #ExceptionTester category: 'BaseImageTests-Exceptions'!
Object subclass: #ExceptionTester
	instanceVariableNames: 'log suiteLog iterationsBeforeTimeout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Exceptions'!
!classDefinition: 'ExceptionTester class' category: 'BaseImageTests-Exceptions'!
ExceptionTester class
	instanceVariableNames: ''!

!classDefinition: #RefactoringClassTestData category: 'BaseImageTests-Tools-Refactoring'!
Object subclass: #RefactoringClassTestData
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-Tools-Refactoring'!
!classDefinition: 'RefactoringClassTestData class' category: 'BaseImageTests-Tools-Refactoring'!
RefactoringClassTestData class
	instanceVariableNames: ''!


!FloatTest commentStamp: 'fbs 3/8/2004 22:13' prior: 0!
I provide a test suite for Float values. Examine my tests to see how Floats should behave, and see how to use them.!

!SmallIntegerTest commentStamp: 'fbs 3/8/2004 22:13' prior: 0!
I provide a test suite for SmallInteger values. Examine my tests to see how SmallIntegers should behave, and see how to use them.!

!ProcessTest commentStamp: 'ul 8/16/2011 11:35' prior: 0!
I hold test cases for generic Process-related behaviour.!

!SemaphoreTest commentStamp: 'tlk 5/5/2006 13:32' prior: 0!
A SemaphoreTest is sunit test for simple and multiEx semaphores

Instance Variables none; does not have common test fixture accross all tests (because its testing differenct sorts of semaphores (could refactor into muliple testcases if there were more test conditions.
!

!BecomeTest commentStamp: '<historical>' prior: 0!
Tests that common uses of #become: and related methods don't crash the system when some method could resume execution on a 'becomed' instance.!

!SpecialSelectorsTest commentStamp: '<historical>' prior: 0!
Tests for proper behavior of Special Selectors, for example #@!

!DecompilerTests commentStamp: 'sd 9/26/2004 13:24' prior: 0!
Apparently the decompiler does not really work totally.
Here are a bunch of methods that can help improving the decompiler:
	- blockingClasses return class for which it is impossible to decompile methods 
	- failures are problems that lead to a DNU
	- decompilerDiscrepancies are the results of running decompileTestHelper..as you see the pattern 	
	is quite present.!

!DecompilerTestFailuresCollector commentStamp: 'jmv 10/16/2013 19:44' prior: 0!
(| dtfc |
dtfc := DecompilerTestFailuresCollector new.
(dtfc class superclass organization listAtCategoryNamed: #tests) do:
	[:s| dtfc perform: s].
dtfc failures)

(Transcript nextPut: ${.
self do: [:mr| Transcript print: mr actualClass; nextPut: $.; space; store: mr methodSymbol; nextPut: $.; cr; flush].
Transcript nextPut: $}; flush)

eem 7/1/2009 16:13
{AdditionalMethodState. #keysAndValuesDo:.
AdditionalMethodState. #propertyKeysAndValuesDo:.
AdditionalMethodState. #at:ifAbsent:.
AdditionalMethodState. #removeKey:ifAbsent:.
AdditionalMethodState. #at:ifAbsentPut:.
AdditionalMethodState. #setMethod:.
AdditionalMethodState. #at:put:.
AdditionalMethodState. #pragmas.
AdditionalMethodState. #includesProperty:.
AdditionalMethodState. #properties.
AdditionalMethodState. #hasLiteralSuchThat:.
AdditionalMethodState. #propertyValueAt:ifAbsent:.
AdditionalMethodState. #hasLiteralThorough:.
Array. #hasLiteralSuchThat:.
BitBltSimulation. #initDither8Lookup.
BlockNode. #sizeCodeExceptLast:.
BlockNode. #emitCodeExceptLast:encoder:.
Categorizer. #changeFromCategorySpecs:.
Categorizer. #elementCategoryDict.
CColorPicker. #colors:.
CCustomDrawListCostume. #drawListOn:in:.
ChangeList. #browseCurrentVersionsOfSelections.
ClosureTests. #testToDoInsideTemp.
Cogit. #computeMaximumSizes.
Cogit. #outputInstructionsAt:.
Cogit. #generateMapAt:start:.
CogVMSimulator. #printFrameThing:at:.
CogVMSimulator. #str:n:cmp:.
CoInterpreter. #validStackPageBaseFrames.
CoInterpreter. #markAndTraceTraceLog.
CoInterpreter. #mapTraceLog.
CoInterpreter. #checkStackIntegrity.
CoInterpreter. #mapStackPages.
CoInterpreter. #updateStackZoneReferencesToCompiledCodePreCompaction.
CoInterpreter. #ceActivateFailingPrimitiveMethod:.
CoInterpreterStackPages. #initializeStack:numSlots:pageSize:stackLimitOffset:stackPageHeadroom:.
CompiledMethod. #=.
CompiledMethod. #getPreambleFrom:at:.
CompiledMethod. #hasLiteralThorough:.
CompiledMethod. #hasLiteralSuchThat:.
CPopUpMenuCostume. #drawMenu:on:in:.
CroquetParticipant. #dropFiles:.
CTextParagraph. #selectionRectsFrom:to:.
CWheelWidgetCostume. #drawOn:in:.
Dictionary. #scanFor:.
Float. #printPaddedWith:to:.
FMSound. #mixSampleCount:into:startingAt:leftVol:rightVol:.
Form. #transformColors:.
FTPClient. #getDataInto:.
GIFReadWriter. #nextImageWithPlugin.
GraphMorph. #drawDataOn:.
GZipReadStream. #on:from:to:.
HTTPServiceDispatcher. #errorReportFor:stack:on:.
HttpUrl. #checkAuthorization:retry:.
Integer. #benchSwitch:.
Interpreter. #primitiveClosureValueWithArgs.
Interpreter. #primitivePerformAt:.
Interpreter. #primitiveDoPrimitiveWithArgs.
Interpreter. #primitiveNewMethod.
InterpreterStackPages. #initializeStack:numSlots:pageSize:stackLimitOffset:stackPageHeadroom:.
JPEGReadWriter. #decodeBlockInto:component:dcTable:acTable:.
KeyedIdentitySet. #scanFor:.
KeyedSet. #scanFor:.
LargeIntegersPlugin. #isNormalized:.
LargeIntegersPlugin. #cBytesCopyFrom:to:len:.
LargeIntegersPlugin. #cDigitMultiply:len:with:len:into:.
LiteralDictionary. #scanFor:.
LoopedSampledSound. #mixSampleCount:into:startingAt:leftVol:rightVol:.
MethodDictionary. #scanFor:.
MP4BoxInfoParser. #parseMP4BoxOutput:.
MP4BoxNHMLTrack. #computeDTSDeltas.
MultiByteBinaryOrTextStream. #next:.
MultiByteFileStream. #next:.
MViewPane. #reconstructEnabledDocuments.
MViewPane. #reconstructOpenDocuments.
MViewPane. #reconstructSelectionList.
TextComposition. #selectionRectsFrom:to:.
Object. #instanceFields.
OldSocket. #getResponseNoLF.
PasteUpMorph. #dropFiles:.
PlotMorphGrid. #bestStep:.
PluckedSound. #reset.
PluggableDictionary. #scanFor:.
PluggableSet. #scanFor:.
PluggableTabButtonMorph. #calculateArcLengths.
PluggableTabButtonMorph. #drawTabOn:.
PNGReadWriter. #copyPixelsGray:.
PNMReadWriter. #readPlainRGB.
PNMReadWriter. #readBWreverse:.
PNMReadWriter. #nextPutRGB:.
PNMReadWriter. #nextPutBW:reverse:.
PopUpMenu. #readKeyboard.
QFloorFan. #initialize.
QMinimalForum. #demoDesksUnused.
QNetVidReorderingBuffer. #popFramesForCTS:.
QNetVidTrackStreamer. #sampleIndexWithCTS:.
QServiceProvider. #statusReport.
QServicesPane. #forumMenuInto:.
QUserListItem. #drawOn:in:.
QVMProfiler. #computeHistograms:.
QVMProfiler. #selectSymbolsInRange.
QwaqParticipantUI. #onDropFiles:.
RelativeInstructionPrinter. #print:.
RemoteHandMorph. #appendNewDataToReceiveBuffer.
SchizophrenicClosureFormatStackInterpreter. #primitiveClosureValueWithArgs.
Set. #do:.
Set. #scanFor:.
SHParserST80. #isBinary.
ShootoutMall. #processVisitors.
ShortIntegerArray. #writeOn:.
SparseLargeArray. #analyzeSpaceSaving.
StackInterpreter. #validStackPageBaseFrames.
StackInterpreter. #divorceAllFrames.
StackInterpreter. #checkStackIntegrity.
StackInterpreter. #primitiveDoPrimitiveWithArgs.
StackInterpreter. #reverseDisplayFrom:to:.
StackInterpreter. #printOop:.
StackInterpreter. #mapStackPages.
StackInterpreter. #primitiveNewMethod.
StackInterpreter. #primitiveClosureValueWithArgs.
StrikeFontSet. #displayStringR2L:on:from:to:at:kern:.
String. #howManyMatch:.
Text. #asHtmlFragmentTextStyle:useBreaks:.
TextURL. #actOnClickFor:.
TFractalTerrain. #heightAt:.
TFractalTerrain. #makeFaces.
TFractalTerrain. #makeVertices.
TFractalTerrain. #makeTextureUV.
TFractalTerrain. #makeVertexNormals.
TFrame. #computeUnionSphere.
TMethod. #emitCCommentOn:.
TRFBStreamOutput. #handleRequest:.
TTCFontReader. #processCharacterMappingTable:.
TTContourConstruction. #segmentsDo:.
TTensor. #projectionIntegrate:.
TTFontReader. #processHorizontalMetricsTable:length:.
TTFontReader. #processCharacterMappingTable:.
TWaves. #step.
Vector. #copyFrom:.
Vector. #asVector3.
VectorColor. #copyFrom:.
WeakKeyDictionary. #scanForNil:.
WeakKeyDictionary. #scanFor:.
WeakSet. #scanFor:.
WeakSet. #scanForLoadedSymbol:.
}!

!FileManTest commentStamp: 'jmv 5/31/2016 10:49' prior: 0!
FileManTest suite run!

!CuisSourceFileArrayTest commentStamp: 'pb 1/24/2016 20:00' prior: 0!
This test documents the source pointer address conversion methods for CuisSourceFileArray.

See class comment there. 
!

!SystemConsistencyTest commentStamp: '<historical>' prior: 0!
General system - wide image health tests.!

!AffineTransformationTest commentStamp: '<historical>' prior: 0!
Tests for AffineTransformation!

!MorphicLocationTest commentStamp: 'jmv 1/14/2015 14:32' prior: 0!
Tests for composition of AffineTransformation and MorphicTranslation!

!MorphicTranslationTest commentStamp: '<historical>' prior: 0!
Tests for MorphicTranslation!

!TaskbarTest commentStamp: '<historical>' prior: 0!
Tests for the Taskbar.!

!WriteStreamTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!TranscriptTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!BecomeTestExperiment commentStamp: '<historical>' prior: 0!
To be used by BecomeTest!

!RefactoringClassTestData class methodsFor: 'compiling' stamp: 'HAW 12/18/2019 19:55:32'!
wantsChangeSetLogging 

	^false! !

!MyResumableTestError methodsFor: 'exceptionDescription' stamp: 'tfei 6/13/1999 00:46'!
isResumable

	^true! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 5/25/2016 14:36'!
testquickFindFirst

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindFirst: [:one | one >= eachElement]) = eachIndex.
			self assert: (collection quickFindFirst: [:one | one >= (eachElement - 0.5)]) = eachIndex
		].
	self assert: (collection quickFindFirst: [:one | one > 2101]) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 5/25/2016 14:36'!
testquickFindLast

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindLast: [:one | one <= eachElement]) = eachIndex.
			self assert: (collection quickFindLast: [:one | one <= (eachElement + 0.5)]) = eachIndex
		].
	self assert: (collection quickFindLast: [:one | one < 0]) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 5/25/2016 14:46'!
testquickIndexOf

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickIndexOf: eachElement) = eachIndex
		].
	self assert: (collection quickIndexOf: 999) = 0.
	self assert: (collection quickIndexOf: 2101) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 5/25/2016 15:01'!
testquickIndexOfWithSemistableOrder

	| collection |
	collection := SortedCollection
		sortBlock: [:x :y | x key <= y key].
	1 to: 1000 do: [:each | collection add: each -> each].
	1 to: 50 do: [:each | collection add: 42 -> each].
	self assert: (collection quickIndexOf: 42 -> 6) = 48! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 11/30/2014 11:34'!
testIdentitySet
	"
	CollectTest  new testIdentitySet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = IdentitySet.
	self assert: result = #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	self assert: result = col! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 11/30/2014 10:51'!
testOrderedCollection
	"
	CollectTest  new testOrderedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asOrderedCollection.
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(2 4 6 8 10) asOrderedCollection! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 11/30/2014 11:34'!
testSet
	"
	CollectTest  new testSet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asSet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = Set.
	self assert: result = #(1 2 3) asSet.
	self assert: result = col! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 11/30/2014 11:01'!
testSortedCollection
	"
	CollectTest  new testSortedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asSortedCollection: [ :a :b | a > b ].
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(10 8 6 4 2) asOrderedCollection! !

!CollectionTest methodsFor: 'groupBy tests' stamp: 'HAW 7/5/2018 15:14:11'!
testGroupByHavingSelectsTheResultOfGroupBy

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ] having: [ :group | group size > 2 ].
	
	self assert: 1 equals: (groupedByEven size).	
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false) ! !

!CollectionTest methodsFor: 'groupBy tests' stamp: 'HAW 7/5/2018 15:15:41'!
testGroupByReturnsADictionaryThatGroupsACollectionByThePluggableKey

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ].
	
	self assert: 2 equals: groupedByEven size.
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger even ]) equals: (groupedByEven at: true).
	self assert:(collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false).! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 11/17/2018 11:45:15'!
testAverageFailsWhenTheCollectionIsEmpty

	self 
		should: [ #() average: [ :each | each ] ]
		raise: Error 
		description: Collection emptyCollectionDescription! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 11/17/2018 11:45:19'!
testAverageIfEmptyCalculatesItAsUsualWhenTheCollectionIsNotEmpty

	| someNumbers |
	
	someNumbers _ #(1 5).

	self 
		assert: (someNumbers average: [ :each | each ] ifEmpty: [ self fail ])
		equals: someNumbers average! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 11/17/2018 11:45:22'!
testAverageIfEmptyEvaluatesEmptyBlockWhenTheCollectionIsEmpty

	| emptyBlock |
	
	emptyBlock _ [ 0 ].

	self 
		assert: (#() average: [ :each | each asInteger ] ifEmpty: emptyBlock)
		equals: emptyBlock value! !

!CollectionTest methodsFor: 'average tests' stamp: 'jmv 1/6/2021 11:57:27'!
testsAverageWorksWithABlock

	self assert: ({ '1' . '2' . '3' } average: [ :each | each asNumber ]) equals: 2! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:11:08'!
assertAsCommaSeparated: aCollection equals: expectedString 
	
	self 
		assert: (String streamContents: [ :stream | aCollection asCommaSeparated: [ :elem | stream print: elem + 1] on: stream ])
		equals: expectedString! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:12:31'!
testAsCommaSeparatedOnWorksAsCommaStringAnd

	self assertAsCommaSeparated: #() equals: ''. 
	self assertAsCommaSeparated: #(1) equals: '2'.
	self assertAsCommaSeparated: #(1 2) equals: '2 and 3'. 
	self assertAsCommaSeparated: #(1 2 3) equals: '2, 3 and 4'.! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:11:51'!
testAsCommaStringAndDoesNotAddAnythingWhenEmpty

	self assert: #() asCommaStringAnd isEmpty ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:11:59'!
testAsCommaStringAndDoesNotAddAnythingWhenHasOneElement

	self assert: #(1) asCommaStringAnd equals: '1' ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:12:06'!
testAsCommaStringAndSeparatesWithAndWhenTwoOneElements

	self assert: #(1 2) asCommaStringAnd equals: '1 and 2' ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 3/6/2019 15:12:21'!
testAsCommaStringAndSeparatesWithCommaButLastOneWithAndWhenMoreThanTwoElements

	self assert: #(1 2 3) asCommaStringAnd equals: '1, 2 and 3' ! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/16/2019 00:08:43'!
testItMaintainsTheSameCollectionSpecies

	self assert: (OrderedCollection with: 1 with: 2) equals: (OrderedCollection with: 1 with: #(2)) flatten! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/15/2019 23:31:15'!
testWhenCollectionContainsASingleLevelOfElementsItReturnsTheSameCollection

	self assert: #(1 2 3) equals: #(1 2 3) flatten! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/16/2019 00:01:02'!
testWhenCollectionContainsMoreThanTwoLevelsltReturnsTheElementsOfAllNestedCollections

	self assert: #(1 2 3 4 5) equals: #(1 #(2 3) #(4 #(5))) flatten! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/16/2019 00:03:22'!
testWhenCollectionContainsStringCollectionsItDoesNotFlattenTheStrings

	self assert: #('string1' 'string2' 'string3') equals: #('string1' #('string2' 'string3')) flatten! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/16/2019 00:02:07'!
testWhenCollectionContainsStringsItReturnsTheSameCollection

	self assert: #('string1' 'string2' 'string3') equals: #('string1' 'string2' 'string3') flatten! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/16/2019 00:03:37'!
testWhenCollectionContainsTwoLevelsOfElementsItReturnsTheElementsOfAllNestedCollections
	
	self assert: #(1 2 3) equals: #(#(1) #(2 3)) flatten
	! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'GC 5/15/2019 23:24:15'!
testWhenCollectionIsEmptyItReturnsTheSameCollection

	self assert: #() equals: #() flatten! !

!CollectionTest methodsFor: 'misc tests' stamp: 'jpb 8/2/2019 23:18:50'!
testIsCollection
	self assert: (Bag new is: #Collection).! !

!CollectionTest methodsFor: 'misc tests' stamp: 'jmv 6/12/2019 18:22:57'!
testSetEquality
	self assert: Set new = IdentitySet new.
	self assert: Set new hash = IdentitySet new hash.
	self assert: Dictionary new = IdentityDictionary new.
	self assert: Dictionary new hash = IdentityDictionary new hash.
	self assert: Dictionary new = OrderedDictionary new.
	self assert: Dictionary new hash = OrderedDictionary new hash.
	self assert: IdentityDictionary new = OrderedDictionary new.
	self assert: IdentityDictionary new hash = OrderedDictionary new hash.

	self deny: Set new = Dictionary new! !

!DictionaryTest methodsFor: 'tests' stamp: 'jpb 8/2/2019 23:17:49'!
testIsCollection
	self assert: (Dictionary new is: #Collection)! !

!DictionaryTest methodsFor: 'tests' stamp: 'HAW 4/4/2019 08:18:05'!
testKeyNotFoundSignalTheRightMessage

	self 
		should: [ Dictionary new errorKeyNotFound ]
		raise: Error
		withMessageText: Dictionary keyNotFoundErrorDescription ! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 12/18/2018 10:56:31'!
testAt
	"
	IntervalTest new testAt
	"
	self assert: (2 to: 5 by: 2) first = 2.
	self assert: ((2 to: 5 by: 2) at: 1) = 2.
	self assert: ((2 to: 5 by: 2) at: 2) = 4.
	self assert: (2 to: 5 by: 2) last = 4.

	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: ((0 to: 2.4 by: 0.1) at: 1) = 0.
	self assert: (((0 to: 2.4 by: 0.1) at: 2) isWithin: 1 floatsFrom: 0.1).
	self assert: (((0 to: 2.4 by: 0.1) at: 3) isWithin: 1 floatsFrom: 0.2).
	self assert: (((0 to: 2.4 by: 0.1) at: 23) isWithin: 1 floatsFrom: 2.2).
	self assert: ((0 to: 2.4 by: 0.1) at: 24) = 2.3.
	self assert: ((0 to: 2.4 by: 0.1) at: 25) = 2.4.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 12/18/2018 10:56:59'!
testFloatInterval
	"
	IntervalTest new testFloatInterval
	"
	
	self assert: (0 to: 2.4 by: 0.1) size = 25.
	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.

	2 to: 200 do: [ :n |
		self assert: (0.0 to: 1.0 count: n) size = n.
		self assert: (0.0 to: 1.0 count: n) first = 0.0.
		self assert: ((0.0 to: 1.0 count: n) last isWithin: 1 floatsFrom: 1.0)].
	2 to: 200 do: [ :n |
		self assert: (1.0 to: 0.0 count: n) size = n.
		self assert: (1.0 to: 0.0 count: n) first = 1.0.
		self assert: ((1.0 to: 0.0 count: n) last isWithin: 1 floatsFrom: 0.0)].! !

!IntervalTest methodsFor: 'testing' stamp: 'HAW 3/17/2019 07:20:11'!
testFloatToByDo
	"
	IntervalTest new testFloatToByDo
	"
	| interval toByDo |

	"See comment at #to:by:do:"
	self shouldFail: [
		interval _ 0 to: 2.4 by: 0.1.
		toByDo _ Array streamContents: [ :strm |
			0 to: 2.4 by: 0.1 do: [ :each |
				strm nextPut: each ]].
		self assert: toByDo size = interval size.
		1 to: toByDo size do: [ :i |
			self assert: (toByDo at: i) = (interval at: i) ]]! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 3/27/2016 17:18'!
testIntegerInterval
	"
	IntervalTest new testIntegerInterval
	"
	| z |
	self assert: (2 to: 5 by: 2) size = 2.
	self assert: (2 to: 5 by: 2) last = 4.
	z _ 0.
	(2 to: 5 by: 2) do: [ :i | z _ i ].
	self assert: z = 4! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 4/27/2016 14:29'!
testIntegerToByDo
	"
	IntervalTest new testIntegerToByDo
	"
	| interval toByDo |
	interval _ 2 to: 5 by: 2.
	toByDo _ Array streamContents: [ :strm |
		2 to: 5 by: 2 do: [ :each |
			strm nextPut: each ]].
	self assert: toByDo size = interval size.
	1 to: toByDo size do: [ :i |
		self assert: (toByDo at: i) = (interval at: i) ]! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:43:33'!
testDoesNotIncludeNumbersOutsideTheInInterval

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self deny: (from1To10 includes: 0).
	self deny: (from1To10 includes: 11)! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:35:03'!
testDoesNotIncludeObjectsThatAreNotNumbers

	self deny: ((1 to: 10) includes: $a)! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:53:40'!
testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStep

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self deny: (from1To10By2 includes: 0).
	self deny: (from1To10By2 includes: 2).
	self deny: (from1To10By2 includes: 6).
	self deny: (from1To10By2 includes: 10).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:54:48'!
testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStepWithReversedIntervals

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self deny: (from10To1ByMinus2 includes: 0).
	self deny: (from10To1ByMinus2 includes: 1).
	self deny: (from10To1ByMinus2 includes: 5).
	self deny: (from10To1ByMinus2 includes: 9).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:43:03'!
testIncludedNumbersRespectAStepDifferentToOne

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self assert: (from1To10By2 includes: 1).
	self assert: (from1To10By2 includes: 5).
	self assert: (from1To10By2 includes: 9).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:55:55'!
testIncludedNumbersRespectNegativeSteps

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self assert: (from10To1ByMinus2 includes: 10).
	self assert: (from10To1ByMinus2 includes: 6).
	self assert: (from10To1ByMinus2 includes: 2).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:43:26'!
testIncludesNumbersInsideTheInterval

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self assert: (from1To10 includes: 1).
	self assert: (from1To10 includes: 5).
	self assert: (from1To10 includes: 10).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:43:21'!
testIncludesNumbersInsideTheIntervalEvenForReversedIntervals

	| from10To1 |
	
	from10To1 := 10 to: 1 by: -1.
	
	self assert: (from10To1 includes: 1).
	self assert: (from10To1 includes: 5).
	self assert: (from10To1 includes: 10).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 12:01:44'!
testIntervalWithOneNumberAndNegativeStepBiggerThanOneIncludeOnlyThatNumber

	| from1To1ByMinus2 |
	
	from1To1ByMinus2 := 1 to: 1 by: -2.
	
	self assert: (from1To1ByMinus2 includes: 1).
	self deny: (from1To1ByMinus2 includes: -1).
	self deny: (from1To1ByMinus2 includes: 3).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 12:01:54'!
testIntervalWithOneNumberAndNegativeStepIncludeOnlyThatNumber

	| from1To1ByMinus1 |
	
	from1To1ByMinus1 := 1 to: 1 by: -1.
	
	self assert: (from1To1ByMinus1 includes: 1).
	self deny: (from1To1ByMinus1 includes: 0).
	self deny: (from1To1ByMinus1 includes: 2).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 12:00:46'!
testIntervalWithOneNumberAndStepBiggerThanOneIncludesOnlyTheNumber

	| from1To1By2 |
	
	from1To1By2 := 1 to: 1 by: 2.
	
	self assert: (from1To1By2 includes: 1).
	self deny: (from1To1By2 includes: -1).
	self deny: (from1To1By2 includes: 3).! !

!IntervalTest methodsFor: 'test - includes' stamp: 'HAW 5/2/2020 11:59:06'!
testIntervalWithOneNumberIncludesOnlyTheNumber

	| from1To1 |
	
	from1To1 := 1 to: 1.
	
	self assert: (from1To1 includes: 1).
	self deny: (from1To1 includes: 0).
	self deny: (from1To1 includes: 2).! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'jpb 8/2/2019 23:18:24'!
testIsCollection
	self assert: (OrderedCollection new is: #Collection).! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'HAW 5/18/2019 17:01:20'!
testStreamContentsWorksAsExpected

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | stream nextPut: 1 ].
	
	self assert: (OrderedCollection with: 1) equals: contents! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'HAW 5/18/2019 17:04:16'!
testWriteStreamOnOrderedCollectionGrowsAsExpected

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | 
		1 to: 101 do: [ :aNumber | stream nextPut: aNumber ]].
	
	1 to: 101 do: [ :aNumber | self assert: aNumber equals: (contents at: aNumber) ]
	! !

!OrderedDictionaryTest methodsFor: 'tests' stamp: 'jmv 5/25/2018 10:36:59'!
testOrder
	| data dict orderingByCollect orderingByDo orderingBySelect aux desiredOrder |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict at: pair first put: pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.


	"Test that order is correct even if different. Also test alternative way to add stuff to dict."
	dict _ OrderedDictionary new.
	data _ self sampleData reversed.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict add: pair first -> pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.
! !

!OrderedDictionaryTest methodsFor: 'tests' stamp: 'jmv 5/25/2018 10:41:04'!
testRemove
	| data dict dataToRemove desiredOrder orderingByDo |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	dataToRemove _ self sampleData2.

	data do: [ :pair |
		dict at: pair first put: pair second ].
	dataToRemove do: [ :pair |
		dict removeKey: pair first ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].

	desiredOrder _ OrderedCollection new.
	data do: [ :pair | desiredOrder add: pair second ].
	dataToRemove do: [ :pair | desiredOrder remove: pair second ].
	
	self assert: orderingByDo = desiredOrder asArray.! !

!OrderedDictionaryTest methodsFor: 'aux' stamp: 'jmv 5/25/2018 10:16:25'!
sampleData
	^ { {1. 'uno'}. {6. 'seis'}. {8. 'ocho'}. {7. 'siete'}. {2. 'dos'}. {3. 'tres'}. {4. 'cuatro'}. {5. 'cinco'}}! !

!OrderedDictionaryTest methodsFor: 'aux' stamp: 'jmv 5/25/2018 10:16:35'!
sampleData2
	^ { {1. 'uno'}. {8. 'ocho'}. {3. 'tres'}}! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 09:49:41'!
testCombinationsAtATimeDoShouldEvaluateBlockWithEmptyCollection
	
	| combinationWasEmpty |
	
	combinationWasEmpty := false.
	'ab' combinations: 0 atATimeDo: [ :combination | combinationWasEmpty := combination isEmpty ].
	
	self assert: combinationWasEmpty! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 09:59:32'!
testCombinationsAtATimeDoWorksAsExpected
	
	| combinations |
	 
	combinations := OrderedCollection new.
	'abc' combinations: 2 atATimeDo: [ :combination | combinations add: combination copy].
	
	self assert: 3 equals: combinations size.
	self assert: (combinations includes: #($a $b)).
	self assert: (combinations includes: #($a $c)).
	self assert: (combinations includes: #($b $c)).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:33:35'!
testDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1.
	traversedElements := OrderedCollection new.
	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ self fail ] ].

	self assert: collectionToTraverse equals: traversedElements! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:38:06'!
testDoSeparatedByDoesNotValueSeparatedBlockOnLastElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1 with: 2.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ traversedElements add: $-] ].

	self assert: (OrderedCollection with: 1 with: $- with: 2) equals: traversedElements! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:29:27'!
testDoSeparatedByDoesNothingForEmptyCollection

	self shouldntFail: [ #() do: [ :anElement | self fail ] separatedBy: [ self fail ] ]! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 17:00:14'!
testFirstAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize

	self assert: #(10 20 30) equals: (#(10 20 30) firstAvailable: 4)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 17:00:43'!
testFirstAvailableReturnsAllObjectsWhenRequestedTheReceiversSize

	self assert: #(10 20 30) equals: (#(10 20 30) firstAvailable: 3)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 16:58:40'!
testFirstAvailableReturnsTheFirstRequestedObjectsWhenTheyAreAvailable

	self assert: #(10 20) equals: (#(10 20 30) firstAvailable: 2)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 17:03:20'!
testLastAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize

	self assert: #(10 20 30) equals: (#(10 20 30) lastAvailable: 4)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 17:02:57'!
testLastAvailableReturnsAllObjectsWhenRequestedTheReceiversSize

	self assert: #(10 20 30) equals: (#(10 20 30) lastAvailable: 3)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 5/21/2020 17:02:27'!
testLastAvailableReturnsTheLastRequestedObjectsWhenTheyAreAvailable

	self assert: #(20 30) equals: (#(10 20 30) lastAvailable: 2)! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:36:00'!
testWithIndexDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ] separatedBy: [ self fail ] ].

	self assert: 1 equals: traversedElements size.
	self assert: $a->1 equals: traversedElements first! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:40:01'!
testWithIndexDoSeparatedByDoesNotValueSeparatedBlockOnLastElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a with: $b.
	traversedElements := OrderedCollection new.

	collectionToTraverse
		withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ]
		separatedBy: [ traversedElements add: $-].

	self assert: 3 equals: traversedElements size.
	self assert: $a->1 equals: traversedElements first.
	self assert: $- equals: traversedElements second.
	self assert: $b->2 equals: traversedElements third! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:30:18'!
testWithIndexDoSeparatedByDoesNothingForEmptyCollection

	self shouldntFail: [ #() withIndexDo: [ :anElement :index | self fail ] separatedBy: [ self fail ] ]! !

!TextTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:40:53'!
testIsText
	self assert: (Text new is: #Text).! !

!WeakIdentitySetTest methodsFor: 'testing' stamp: 'jmv 8/24/2012 08:24'!
test
	"
	(also tests WeakSet a bit)
	WeakIdentitySetTest new test
	"
	| ary1 ary2 count w wi |
	ary1 _ { 3@4 . 4@5 }.
	ary2 _ { 13@4 . 14@5 }.
	w _ WeakSet new.
	w addAll: ary1.
	wi _ WeakIdentitySet new.
	wi addAll: ary2.
	
	self assert: w size = 2.
	count _ 0.
	w do: [ :each |
		count _ count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (w includes: ary1 first).
	self assert: (w includes: ary1 second).
	self assert: (w includes: 3@4).
	self assert: (w includes: 4@5).

	self assert: wi size = 2.
	count _ 0.
	wi do: [ :each |
		count _ count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).
	self deny: (wi includes: 13@4).
	self deny: (wi includes: 14@5).

	"Now make one element in each disappear"
	ary1 at: 1 put: 9.
	ary2 at: 1 put: 99.
	Smalltalk garbageCollect.

	"A little weird, but yes, elements that disappeared are still counted, but NOT iterated!!"
	self assert: w size = 2.
	count _ 0.
	w do: [ :each |
		count _ count + 1 ].
	self assert: count = 1.
	self deny: (w includes: ary1 first).
	self assert: (w includes: ary1 second).

	self assert: wi size = 2. 	"A little weird, but yes, elements that disappeared are still counted"
	count _ 0.
	wi do: [ :each |
		count _ count + 1 ].
	self assert: count = 1.
	self deny: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HernanWilkinson 1/19/2017 20:32:36'!
testAllSuperclassesUpToFailsWhenWantsToStopOnInvalidSuperclass

	| invalidSuperclass |
	
	invalidSuperclass := Collection.
	self
		should: [ SmallInteger allSuperclassesUpTo: invalidSuperclass ]
		raise: Error
		withExceptionDo: [:anError | 
			self assert: anError messageText equals: (nil superclassNotValidErrorDescriptionFor: invalidSuperclass) ]
		
	! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 2/10/2017 10:51:40'!
testAllSuperclassesUpToPutsFirstSuperclassesFirst

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Number.
	
	self assert: 2 equals: superclasses size.
	self assert: Integer equals: superclasses first.
	self assert: Number equals: superclasses second! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 2/10/2017 10:52:55'!
testAllSuperclassesUpToStopsOnRightClass

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Integer.
	
	self assert: 1 equals: superclasses size.
	self assert: (superclasses includes: Integer)
	
	! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 8/3/2019 09:29:49'!
testHighestClassImplementingReturnsReceiverWhenNoOtherSuperclassImplementsSelector

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #addFirst: ifNone: [ self fail ].
	
	self assert: OrderedCollection equals: higestClass 
	! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 8/3/2019 09:30:01'!
testHighestClassImplementingReturnsTheTopSuperclassImplementingSelector

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #select: ifNone: [ self fail ].
	
	self assert: Collection equals: higestClass 
	! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 8/3/2019 09:29:55'!
testHighestClassImplementingValuesIfNoneBlockWhenSelectorIsNotImplemented

	OrderedCollection highestClassImplementing: #x ifNone: [ ^self  ].
	
	self fail! !

!BehaviorTest methodsFor: 'class hierarchy tests' stamp: 'HAW 2/10/2017 10:52:30'!
testWithAllSuperclassesUpToIncludesReceiverClass

	| superclasses |
	
	superclasses := SmallInteger withAllSuperclassesUpTo: Number.
	
	self assert: 3 equals: superclasses size.
	self assert: SmallInteger equals: superclasses first.
	self assert: Integer equals: superclasses second.
	self assert: Number equals: superclasses third ! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 6/28/2020 11:57:28'!
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

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 4/9/2020 16:17:49'!
testAllAccessToIsEmptyForVariableNotDefinedInClass

	self assert: (Object allAccessesTo: 'iv1') isEmpty! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 6/28/2020 11:53:39'!
testAllBroadAccessToReturnsAccessInAllHierarchy

	| references |

	"Just in case the hierarchy changes... not much of a chance but... - Hernan"
	self deny: (IdentitySet includesBehavior: Dictionary).
	self deny: (Dictionary includesBehavior: IdentitySet).
	self assert: (IdentitySet includesBehavior: Set).
	self assert: (Dictionary includesBehavior: Set).

	references := IdentitySet allBroadAccessesTo: 'array'.

	self assert: (references anySatisfy: [ :aReference | aReference methodClass = Dictionary ])! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 8/2/2018 16:54:42'!
testAllUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables

	| unreferencedVariables |
	
	unreferencedVariables := BehaviorTestSubclass allUnreferencedInstanceVariables.
	
	self assert: 1 equals: unreferencedVariables size.
	self assert: (unreferencedVariables includes: 'unreferenced') ! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 8/2/2018 16:54:45'!
testHasReferencesToInstanceVariableNamedLooksInClassOnly

	self deny: (self class hasReferencesToInstanceVariableNamed: 'referenced').
	self assert: (BehaviorTestSubclass hasReferencesToInstanceVariableNamed: 'referenced').
	! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 8/2/2018 16:54:48'!
testIsInstanceVariableNamedReferencedInHierarchyLooksReferencesInHierarchy

	self assert: (self class isInstanceVariableNamedReferencedInHierarchy: 'referenced').
	self deny: (self class isInstanceVariableNamedReferencedInHierarchy: 'unreferenced').
	! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 6/28/2020 11:48:19'!
testProtoObjectAllRegularInstVarNamesDoesNotFail

	self
		shouldnt: [ ProtoObject allRegularInstVarNames ]
		raise: Error! !

!BehaviorTest methodsFor: 'instance variables tests' stamp: 'HAW 8/2/2018 16:54:52'!
testUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables

	| unreferencedVariables |
	
	unreferencedVariables := self class unreferencedInstanceVariables.
	
	self assert: 1 equals: unreferencedVariables size.
	self assert: (unreferencedVariables includes: 'unreferenced') ! !

!BehaviorTestSubclass methodsFor: 'test data' stamp: 'HAW 8/2/2018 16:54:29'!
methodReferencingReferencedVariable

	^referenced  ! !

!BehaviorTestSubclass class methodsFor: 'as yet unclassified' stamp: 'HAW 2/28/2019 19:18:42'!
isAbstract

	^true! !

!CategorizerTest methodsFor: 'assertions' stamp: 'HAW 9/24/2020 19:46:47'!
assertAddsWithoutBlanks: aCategoryWithBlanks

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer addCategory: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	
	
	! !

!CategorizerTest methodsFor: 'assertions' stamp: 'HAW 9/24/2020 19:46:54'!
assertClassifiesWithoutBlanks: aCategoryWithBlanks

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer classify: 1 under: aCategoryWithBlanks suppressIfDefault: false.
	
	self assert: (categorizer categoryOfElement: 1) equals: aCategoryWithBlanks withBlanksTrimmed 
	! !

!CategorizerTest methodsFor: 'assertions' stamp: 'HAW 9/24/2020 19:49:53'!
assertRenamesToWithoutBlanks: aCategoryWithBlanks

	| categorizer categoryToRename |
	
	categorizer := self createCategorizer.
	
	categoryToRename := 'someCategory'.
	categorizer classify: 1 under: categoryToRename suppressIfDefault: false.
	categorizer renameCategory: categoryToRename toBe: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	self deny: (categorizer categories includes: categoryToRename).
	self assert: aCategoryWithBlanks withBlanksTrimmed equals: (categorizer categoryOfElement: 1)  
	! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:57:50'!
testClassifiesWithoutLeadingBlanks

	self assertClassifiesWithoutBlanks: self categoryWithLeadingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:57:33'!
testClassifiesWithoutTrailingBlanks

	self assertClassifiesWithoutBlanks: self categoryWithTrailingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:56:18'!
testRemovesLeadingBlanksWhenAddingCategory

	self assertAddsWithoutBlanks: self categoryWithLeadingBlanks.
	
	! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:55:58'!
testRemovesTrailingBlanksWhenAddingCategory

	self assertAddsWithoutBlanks: self categoryWithTrailingBlanks.
	
	! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:58:11'!
testRenamesWithoutLeadingBlanks

	self assertRenamesToWithoutBlanks: self categoryWithLeadingBlanks ! !

!CategorizerTest methodsFor: 'tests' stamp: 'HAW 9/24/2020 19:58:01'!
testRenamesWithoutTrailingBlanks

	self assertRenamesToWithoutBlanks: self categoryWithTrailingBlanks ! !

!CategorizerTest methodsFor: 'support' stamp: 'HAW 9/24/2020 16:41:35'!
categoryWithLeadingBlanks

	^ ' withLeadingBlanks'! !

!CategorizerTest methodsFor: 'support' stamp: 'HAW 9/24/2020 16:41:23'!
categoryWithTrailingBlanks

	^ 'withTrailingBlanks '! !

!CategorizerTest methodsFor: 'support' stamp: 'HAW 9/24/2020 19:46:47'!
createCategorizer

	^ Categorizer defaultList: #()! !

!ClassOrganizerTest methodsFor: 'support' stamp: 'HAW 9/24/2020 19:53:00'!
createCategorizer

	^ ClassOrganizer defaultList: #()! !

!ClassTest methodsFor: 'definition tests' stamp: 'HAW 10/24/2019 09:34:44'!
testDefinitionReplacingCategoryWithReplacesTheCategoryWithTheGivenOne

	| classDefinition newCategory newCategoryDefinition |
	
	newCategory := 'NewCategory'.
	newCategoryDefinition := 'category: ''', newCategory, ''''.
	classDefinition := self class definitionReplacingCategoryWith: newCategory.
	
	self assert: (classDefinition includesSubString: newCategoryDefinition).
	self deny: (classDefinition includesSubString: self class category)
		

! !

!ClassTest methodsFor: 'definition tests' stamp: 'HAW 10/24/2019 09:42:54'!
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

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/18/2018 16:27:22'!
effectiveZeroExponentForTrigonometry

	^-50! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 01:19:37'!
hyperbolicPrecision

	^16! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 01:12:19'!
trigonometricDegreePrecision

	^4! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 00:25:51'!
trigonometricPrecision

	^3! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 00:22:44'!
verify: aFloat is: anInteger floatsAwayFrom: anotherFloat

	self assert: (aFloat floatsAwayFrom: anotherFloat) = anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) = anInteger negated.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) = anInteger negated.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) = anInteger! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/18/2018 16:27:33'!
verify: aFloat isDegreeTrigonometricallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricDegreePrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/18/2018 16:27:38'!
verify: aFloat isHyperbolicallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self hyperbolicPrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/18/2018 16:27:41'!
verify: aFloat isTrigonometricallyEqualTo: anotherFloat

	aFloat = 0.0 ifTrue: [
		anotherFloat = 0.0 ifTrue: [^self ].
		^self assert: anotherFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	anotherFloat = 0.0 ifTrue: [^self assert: aFloat asFloat exponent <= self effectiveZeroExponentForTrigonometry].
	self verify: aFloat asFloat isWithin: self trigonometricPrecision floatsAwayFrom: anotherFloat asFloat! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 00:22:57'!
verify: aFloat isWithin: anInteger floatsAwayFrom: anotherFloat

	self assert: (aFloat floatsAwayFrom: anotherFloat) abs <= anInteger.
	self assert: (anotherFloat floatsAwayFrom: aFloat) abs <= anInteger.
	self assert: (aFloat negated floatsAwayFrom: anotherFloat negated) abs <= anInteger.
	self assert: (anotherFloat negated floatsAwayFrom: aFloat negated) abs <= anInteger! !

!AbstractNumberTest methodsFor: 'helpers' stamp: 'jmv 12/18/2018 10:40:29'!
verify: aFloat isWithinOneFloatAwayFrom: anotherFloat

	self verify: aFloat isWithin: 1 floatsAwayFrom: anotherFloat! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 2/21/2019 17:38:32'!
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

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 11:56:44'!
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

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 2/21/2019 17:53:58'!
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

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 2/21/2019 17:41:52'!
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

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 10/15/2019 16:32:51'!
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

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 3/26/2019 10:59:17'!
testTimesTwoPowerOverflow
	self assert: (Float fminNormalized timesTwoPower: Float emax - Float emin) equals: (2.0 raisedTo: Float emax).
	self assert: (Float zero timesTwoPower: SmallInteger maxVal) equals: Float zero.
	self assert: (Float zero timesTwoPower: SmallInteger maxVal squared) equals: Float zero.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal) equals: Float infinity.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float infinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal) equals: Float negativeInfinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float negativeInfinity.! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 3/26/2019 10:58:50'!
testTimesTwoPowerUnderflow
	self assert: ((2.0 raisedTo: Float emax) timesTwoPower: Float emin - Float emax) equals: Float fminNormalized.
	self assert: (Float infinity timesTwoPower: SmallInteger minVal * SmallInteger maxVal) equals: Float infinity.
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal squared negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal squared negated).
! !

!FloatTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 12:03:05'!
testZeroDividedByZero

	self assert: (0.0 / 0.0) isNaN.
	self assert: (0.0 / -0.0) isNaN.
	self assert: (-0.0 / 0.0) isNaN.
	self assert: (-0.0 / -0.0) isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:56:51'!
testArCosh
	self assert: 1.0 arCosh = 0.0.
	self deny: Float infinity arCosh isFinite.
	self verify: 2.5 arCosh cosh isHyperbolicallyEqualTo: 2.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 3/26/2019 11:23:12'!
testArSinh
	self assertIsPositiveZero: 0.0 arSinh.
	self assertIsNegativeZero: Float negativeZero arSinh.
	self deny: Float infinity arSinh isFinite.
	self verify: 0.5 arSinh negated isHyperbolicallyEqualTo: 0.5 negated arSinh.
	self verify: 0.5 arSinh sinh isHyperbolicallyEqualTo: 0.5.
	self verify: -2.5 arSinh sinh isHyperbolicallyEqualTo: -2.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:57:47'!
testArTanh
	self assertIsPositiveZero: 0.0 arTanh.
	self assertIsNegativeZero: Float negativeZero arTanh.
	self deny: 1 arTanh isFinite.
	self verify: 0.5 arTanh negated isHyperbolicallyEqualTo: 0.5 negated arTanh.
	self verify: 0.5 arTanh tanh isHyperbolicallyEqualTo: 0.5.
	self verify: -0.5 arTanh tanh isHyperbolicallyEqualTo: -0.5! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 10:52:08'!
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

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 13:11:53'!
testArcTanNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 2.0 5.0 20.0] do: [ :x | | computed |
		computed _ x arcTanNonPrimitive.
		self assert: (computed tan -x ) abs <= (computed predecessor tan -x ) abs.
		self assert: (computed tan -x ) abs <= (computed successor tan -x ) abs.
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:48:21'!
testCos

	self verify: 0.0 cos isTrigonometricallyEqualTo: 1.0.
	self verify: (Float pi / 3) cos isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) cos isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 6) cos isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) cos isTrigonometricallyEqualTo: 0.0! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:58:14'!
testCosh
	self verify: 0.0 cosh isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh isHyperbolicallyEqualTo: 2.0 negated cosh.
	self deny: Float infinity cosh isFinite	! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/18/2018 16:27:57'!
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

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'nice 5/3/2014 22:30:50.872'!
testDegreeCosForExceptionalValues
	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float negativeInfinity degreeCos isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/18/2018 16:28:02'!
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

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'nice 5/3/2014 22:30:55.613'!
testDegreeSinForExceptionalValues
	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float negativeInfinity degreeSin isNaN.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 13:15:27'!
testExp
	self assert: 0.0 exp = 1.0.
	self assert: 1.0 exp = Float e.
	self assert: 2.0 exp sqrt = Float e! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 13:28:53'!
testExpNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 2.0 5.0 ] do: [ :x | | computed expected |
		computed _ x expNonPrimitive.
		expected _ x exp.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/12/2018 14:38:16'!
testFloorLog2
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."
	
	| aBitLess aBitMore |
	aBitMore := 1.0 successor.
	aBitLess := 1.0 predecessor.
	Float emin + 1 to: Float emax - 1 do: [:exp |
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: exp equals: (exactPowerOfTwo floorLog: 2).
		self assert: exp equals: (exactPowerOfTwo * aBitMore floorLog: 2).
		self assert: exp-1 equals: (exactPowerOfTwo * aBitLess floorLog: 2) ].! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 13:33:48'!
testLn
	self assert: 0.0 exp ln = 0.0.
	self assert: 1.0 exp ln = 1.0.
	self assert: 2.0 exp ln = 2.0.! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/14/2018 23:53:24'!
testLnForExceptionalValues

	self assert: -1.0 ln isNaN.
	self assert: Float infinity ln = Float infinity.
	self assert: Float negativeInfinity ln isNaN! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/11/2018 13:39:49'!
testLnNonPrimitive

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.01 2.01 5.0 ] do: [ :x | | computed expected |
		computed _ x exp lnNonPrimitive.
		expected _ x exp ln.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 7/2/2019 11:55:02'!
testReciprocal

	self 
		assert: 1.0 reciprocal = 1.0;
		assert: 2.0 reciprocal = 0.5;
		assert: -1.0 reciprocal = -1.0;
		assert: -2.0 reciprocal = -0.5.
		
	self should: [ 0.0 reciprocal ] raise: ZeroDivide.
	self assert:  0.0 reciprocal isExactly: Float infinity! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:47:34'!
testSin

	self verify: 0.0 sin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) sin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) sin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) sin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) sin isTrigonometricallyEqualTo: 1.0! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/11/2018 13:41:55'!
testSinNonPrimitive
	self assert: 0.0 sinNonPrimitive = 0.0.
	self assert: (45 degreesToRadians sinNonPrimitive - 0.5 sqrt) abs <= (0.5 ulp * 2).
	self assert: (90 degreesToRadians sinNonPrimitive - 1.0) abs <= (1.0 ulp * 2).! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:58:36'!
testSinh
	self assertIsPositiveZero: 0.0 sinh.
	self assertIsNegativeZero: Float negativeZero sinh.
	self deny: Float infinity sinh isFinite.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 sinh negated isHyperbolicallyEqualTo: 2.0 negated sinh! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 3/26/2019 16:00:43'!
testSqrt
	self assert: Float nan sqrt isNaN.
	self assert: Float infinity sqrt = Float infinity.
	self assertIsPositiveZero: Float zero sqrt.
	self assertIsNegativeZero: Float negativeZero sqrt.
	self assert: Float fminNormalized sqrt squared = Float fminNormalized .
	self assert: Float fminDenormalized sqrt squared = Float fminDenormalized .! !

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 11:03:49'!
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

!FloatTest methodsFor: 'tests - mathematical functions' stamp: 'sqr 12/15/2018 00:58:54'!
testTanh
	self assertIsPositiveZero: 0.0 tanh.
	self assertIsNegativeZero: Float negativeZero tanh.
	self assert: Float infinity tanh = 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 tanh negated isHyperbolicallyEqualTo: 2.0 negated tanh! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'nice 7/10/2009 22:27'!
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

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 10/8/2018 16:20:48'!
testComparisonWithFraction
	self assert: 0.5 = (1/2).
	self assert: (1/2) = 0.5.
	self assert: 0.1 = (1/10).
	self assert: (1/10) = 0.1! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 8/29/2019 16:20:39'!
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
		((Set new: 3) add: 3; add: 3.0; size) =
		((Set new: 4) add: 3; add: 3.0; size).


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

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 12/4/2018 12:05:21'!
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

!FloatTest methodsFor: 'tests - comparing' stamp: 'sqr 12/14/2018 23:02:34'!
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

!FloatTest methodsFor: 'tests - comparing' stamp: 'sqr 12/14/2018 23:02:43'!
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

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 10/8/2018 16:18:13'!
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

!FloatTest methodsFor: 'tests - comparing' stamp: 'nice 10/27/2014 21:50'!
testLiteralEqualityOfNan
	| nan |
	nan := Float nan.
	self assert: (nan literalEqual: nan)
		description: 'Float nan is not equal to itself, though it is literally equal'.! !

!FloatTest methodsFor: 'tests - comparing' stamp: 'jmv 10/3/2018 16:29:31'!
testLiteralEqualityOfZeroAndNegativeZero
	self assert: Float zero = Float negativeZero.
	self deny: (Float zero literalEqual: Float negativeZero).
	self
		assert: (Compiler evaluate: '1>2 ifTrue: [0.0] ifFalse: [-0.0]') hex = Float negativeZero hex
		description: 'Float zero and negativeZero are not literally substituable'.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 3/27/2019 15:10:58'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 12/4/2018 12:06:58'!
testCeiling
	self assert: 1.0 ceiling = 1.
	self assert: 1.0 successor ceiling = 2.
	self assert: 1.0 predecessor ceiling = 1.
	self assert: 1.1 ceiling = 2.
	self assert: -2.0 ceiling = -2.
	self assert: -2.1 ceiling = -2.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 7/24/2008 02:04'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 4/26/2006 05:21'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 12/4/2018 12:07:13'!
testFloor
	self assert: 1.0 floor = 1.
	self assert: 1.0 successor floor = 1.
	self assert: 1.0 predecessor floor = 0.
	self assert: 1.1 floor = 1.
	self assert: -2.0 floor = -2.
	self assert: -2.1 floor = -3.! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 3/28/2016 09:48'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 1/10/2007 02:29'!
testFractionAsFloat2
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction = ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction = ((1<<52)+2).! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/10/2018 16:47:20'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/3/2018 16:22:06'!
testFractionAsFloatWithUnderflow
	"test rounding to nearest even"

	| underflowPower |
	underflowPower := Float emin - Float precision.
	self assertIsPositiveZero: (2 raisedTo: underflowPower) asFloat.	
	self assertIsNegativeZero: (2 raisedTo: underflowPower) negated asFloat! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 5/6/2006 22:13'!
testIntegerAsFloat
	"assert IEEE 754 round to nearest even mode is honoured"
	
	self deny: 16r1FFFFFFFFFFFF0801 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 65 bits"
	self deny: 16r1FFFFFFFFFFFF0802 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 64 bits"
	self assert: 16r1FFFFFFFFFFF1F800 asFloat = 16r1FFFFFFFFFFF20000 asFloat. "nearest even is upper"
	self assert: 16r1FFFFFFFFFFFF0800 asFloat = 16r1FFFFFFFFFFFF0000 asFloat. "nearest even is lower"
! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/24/2019 11:56:51'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/24/2019 12:01:05'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/9/2018 16:12:35'!
testRoundHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: 0.5 rounded = 0.
	self assert: 1.5 rounded = 2.
	self assert: -0.5 rounded = -0.
	self assert: -1.5 rounded = -2.
	self assert: (((0.0 to: 5.0 by: 0.25) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((-0.0 to: -5.0 by: -0.25) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) )! !

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'nice 6/3/2011 21:37'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/10/2018 16:53:57'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 10/10/2018 17:17:33'!
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

!FloatTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 12/4/2018 12:06:36'!
testTruncated
	self assert: 1.0 truncated = 1.
	self assert: 1.0 successor truncated = 1.
	self assert: 1.0 predecessor truncated = 0.
	self assert: 1.1 truncated = 1.
	self assert: -2.0 truncated = -2.
	self assert: -2.1 truncated = -2.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/21/2018 11:36:52'!
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

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/21/2018 14:56:27'!
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

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 9/24/2018 10:51:07'!
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

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/4/2018 12:09:53'!
testExactStoreString2
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/21/2018 11:40:15'!
testFromStringDoesNotUnderflow
	self assert: '12345678901234567890.0e-330' asNumber isZero not.
	self assert: (Compiler evaluate: '12345678901234567890.0e-330')isZero not.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/21/2018 11:44:33'!
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

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/21/2018 12:15:04'!
testPrintShowingDecimalPlaces4
	| requiredDigits |
	requiredDigits _ Float pi printString size - 2.
	self assert: (Float pi printStringFractionDigits: requiredDigits) = Float pi printString.
	0 to: 100 do: [ :extra |
		self assert: (Float pi printStringFractionDigits: requiredDigits + extra) asNumber = Float pi ]! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/27/2018 16:50:31'!
testPrintStringStress
	| eWidth mWidth ePrimeScale mPrimeScale |
	eWidth _ Float emax - Float emin.
	"211 for a more exhaustive but slower test. 41 for a quick run."
	ePrimeScale _ 211.
	ePrimeScale _ 41.
	mWidth _ 1 bitShift: Float precision.
	"9931 for a more exhaustive but slower test. 37 for a quick run."
	mPrimeScale _ 9931.
	mPrimeScale _ 37.
	0
		to: mWidth - 1 * mPrimeScale
		by: mWidth
		do: [ :mScaled | | m eAdjustment |
			m _ mScaled // mPrimeScale.
			eAdjustment _ m highBit - 1.
			{mScaled. m. m hex. eAdjustment } print.
			Float emin * ePrimeScale
				to: Float emax * ePrimeScale
				by: eWidth
				do: [ :eFraction | | e fp |
					e _ eFraction // ePrimeScale - eAdjustment.
					fp _ m asFloat timesTwoPower: e.
					self assert: fp printString asNumber = fp.
					self assert: fp negated printString asNumber = fp negated ]]! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'nice 3/14/2008 23:59'!
testReadFromManyDigits
	"A naive algorithm may interpret these representations as Infinity or NaN.
	This is http://bugs.squeak.org/view.php?id=6982"
	
	| s1 s2 |
	s1 := '1' , (String new: 321 withAll: $0) , '.0e-321'.
	s2 := '0.' , (String new: 320 withAll: $0) , '1e321'.
	self assert: (Number readFrom: s1) = 1.
	self assert: (Number readFrom: s2) = 1.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 10/19/2018 16:33:22'!
testSomeNines
	| twelveNines |
	twelveNines _ Compiler evaluate: '999999 + 0.999999'.
	self assert: twelveNines printString = '999999.999999'.
	self assert: twelveNines printString asNumber = twelveNines.
	self assert: twelveNines printString asNumber - twelveNines = 0.0.
	self assert: twelveNines + 1e-6 = 1e6! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'nice 10/11/2008 21:45'!
testStoreBase16
	"This bug was reported in mantis http://bugs.squeak.org/view.php?id=6695"

	self
		assert: (20.0 storeStringBase: 16) = '16r14.0'
		description: 'the radix prefix should not be omitted, except in base 10'! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/4/2018 12:09:57'!
testStoreOn
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.! !

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'dtl 9/18/2004 12:40'!
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

!FloatTest methodsFor: 'tests - conversion from to String' stamp: 'jmv 12/12/2018 14:44:07'!
testStringAsNumber2
	"Check that small but normal Floats can be created from Strings"

	| aFloat |
	aFloat _ '12345678901234567890.0e-326' asNumber.
	self deny: aFloat = 0.0.
	self assert: aFloat > 0.0.
	self assert: aFloat - 1.2345678901234568e-307 = 0.
	self assert: aFloat = 1.2345678901234568e-307.! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 14:02:56'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 14:01:27'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 10:43:35'!
testHashWithFraction

	| a float fraction |
	fraction _ 1/3.
	float _ (1/3) asFloat.
	self assert: fraction = float description: 'precondition'.
	self assert: float = fraction description: 'precondition'.
	a _ Set new.
	a add: fraction; add: float.
	self assert: a size = 1! !

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 10:51:32'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 10:51:37'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 10:51:40'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/10/2018 15:40:00'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/9/2018 10:51:44'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/10/2018 15:40:11'!
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

!FloatTest methodsFor: 'tests - hash' stamp: 'jmv 10/10/2018 15:57:54'!
testNoIntegerHash

	self deny: Float pi hash = 3 hash.
	self deny: Float pi hash = (Float pi *1.00000000000001) hash.
	self deny: Float pi hash = (Float pi /1.00000000000001) hash.! !

!FloatTest methodsFor: 'tests - hash' stamp: 'nice 2/13/2010 04:15'!
testSetOfFloat
	"Classical disagreement between hash and = did lead to a bug.
	This is a non regression test from http://bugs.squeak.org/view.php?id=3360"

	| size3 size4 |
	size3 := (Set new: 3) add: 3; add: 3.0; size.
	size4 := (Set new: 4) add: 3; add: 3.0; size.
	self assert: size3 = size4 description: 'The size of a Set should not depend on its capacity.'! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 12/27/2018 18:37:19'!
testE
	"Just in case..."

	self assert: Float e = 2.718281828459045.
	self assert: Float e ln = 1.0.
	self assert: Float e = 1.0 exp.
	self assert: Float e hex = '4005BF0A8B145769'.! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 10/11/2011 08:55'!
testMaxExactInteger
	"
	FloatTest new testMaxExactInteger
	"

	self assert: Float maxExactInteger asFloat truncated = Float maxExactInteger.
	0 to: 10000 do: [ :j |
		self assert: (Float maxExactInteger-j) asFloat truncated = (Float maxExactInteger-j) ].
	self deny: (Float maxExactInteger+1) asFloat truncated = (Float maxExactInteger+1)
	! !

!FloatTest methodsFor: 'tests - constants' stamp: 'jmv 12/27/2018 18:39:24'!
testPi
	"Just in case..."

	self assert: Float pi = 3.141592653589793.
	self assert: Float pi = (1.0 arcTan * 4).
	self assert: Float pi hex = '400921FB54442D18' ! !

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 4/5/2019 16:23:22'!
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

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 12/4/2018 15:20:59'!
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

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 12/4/2018 15:16:45'!
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

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 4/5/2019 16:26:34'!
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

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 12/4/2018 17:10:45'!
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

!FloatTest methodsFor: 'tests - precision and extreme values' stamp: 'jmv 12/12/2018 14:27:52'!
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

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 4/5/2019 16:25:12'!
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

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 4/5/2019 19:51:34'!
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

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 12/4/2018 14:10:06'!
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

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'nice 5/3/2014 22:31:57.837'!
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

!FloatTest methodsFor: 'tests - accessing parts' stamp: 'jmv 10/3/2018 17:41:49'!
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

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'nice 1/27/2017 22:01'!
testCopySign
	self assert: (0.0 copySignTo: 1) = 1.
	self assert: (Float negativeZero copySignTo: 1) = -1.
	self assertIsNegativeZero: (-1 copySignTo: 0.0).
	self assertIsPositiveZero: (1 copySignTo: Float negativeZero).! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/4/2019 14:27:15'!
testIsZero
	self assert: 0.0 isZero.
	self assert: -0.0 isZero.
	self deny:  0.1 isZero.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/5/2019 08:27:21'!
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

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/4/2019 14:32:48'!
testZeroAbs
	self assertIsPositiveZero: Float zero abs. 'the absolute value of a positive zero is zero'.
	self assertIsPositiveZero: Float negativeZero abs. 'the absolute value of a negative zero is zero'.! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 7/2/2019 11:52:32'!
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

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/4/2019 14:28:40'!
testZeroSign
	self assert: Float zero sign = 0.
	self assert: Float negativeZero sign = -1 description: 'negative zero is a zero with non zero sign!!'! !

!FloatTest methodsFor: 'tests - zero behavior' stamp: 'jmv 4/4/2019 14:33:40'!
testZeroSignificandAsInteger
	"This is about http://bugs.squeak.org/view.php?id=6990"

	self assert: 0.0 significandAsInteger = 0.
	self assert: -0.0 significandAsInteger = 0! !

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'sd 6/5/2005 08:30'!
testInfinity1
   "FloatTest new testInfinity1"

	| i1  i2 |

	i1 := 10000 exp.
	i2 := 1000000000 exp.
	self assert: i1 isInfinite & i2 isInfinite & (i1 = i2).
	"All infinities are equal. (This is a very substantial difference to NaN's, which are never equal."
! !

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'sd 6/5/2005 08:30'!
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

!FloatTest methodsFor: 'tests - infinity behavior' stamp: 'jmv 5/21/2020 22:51:06'!
testInfinityTruncated
	self assert: Float infinity truncated isInfinite.
	self assert: Float negativeInfinity truncated isInfinite.! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'sd 6/5/2005 08:31'!
testNaN1
   	"FloatTest new testNaN1"

	self assert: Float nan == Float nan.
	self deny: Float nan = Float nan.
	"a NaN is not equal to itself."
! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 4/4/2019 14:20:45'!
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

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 4/4/2019 14:21:07'!
testNaN3
	"FloatTest new testNaN3"

	| set item identitySet |
	set _ Set new.
	set add: (item _ Float nan).
	self deny: (set includes: item).
	identitySet _ IdentitySet new.
	identitySet add: (item _ Float nan).
	self assert: (identitySet includes: item).! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 4/23/2019 18:20:18'!
testNaN4
	"FloatTest new testNaN4"

	| dict |
	dict _ Dictionary new.
	dict
		at: Float nan
		put: #NaN.
	self deny: (dict includesKey: Float nan).! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 3/13/2012 12:34'!
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

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 8/1/2019 18:12:56'!
testNaNPropagationBinary
	"Any operation should propagate NaN"
	| s a c |
	s _ Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s _ s select: [ :sel | sel numArgs = 1 ].
	s removeAll: #(#at: #basicAt: #ifNotZero: smoothIsAbsBelow:).
	a _ s asArray sort.
	a do: [ :sel |
		{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op1 |
			{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op2 |
				op1 isNaN | op2 isNaN ifTrue: [
					c _ [ op1 perform: sel with: op2 ] on: Error do: [ #error ].
					self assert: (c isNumber not or: [c isNaN]) ]]]].! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 8/1/2019 18:39:54'!
testNaNPropagationUnary
	"Any operation should propagate NaN"
	| s a c |
	s _ Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s _ s select: [ :sel | sel numArgs = 0 ].
	s removeAll: #(#asIEEE32BitWord #byteSize #hash #identityHash #mantissaBits #signBit #smoothStep #exponentBits #imaginary #exponent primTestExponent).
	a _ s asArray sort.
	a do: [ :sel |
		c _ [ Float nan perform: sel ] on: Error do: [ #error ].
		self assert: (c isNumber not or: [c isNaN]) ].! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'jmv 5/21/2020 22:51:38'!
testNaNTruncated
	self assert: Float nan truncated isNaN.! !

!FloatTest methodsFor: 'tests - NaN behavior' stamp: 'nice 3/14/2008 23:42'!
testNaNisLiteral
	self deny: Float nan isLiteral description: 'there is no literal representation of NaN'! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/4/2019 14:02:34'!
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

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 9/3/2020 17:12:44'!
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

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'nice 5/30/2006 02:34'!
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

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/4/2019 14:15:16'!
test32bitInfinities
	| infinityBits negativeInfinityBits |
	infinityBits _ '01111111100000000000000000000000'.
	self assert: (Float infinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = infinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: infinityBits readStream base: 2)) hex = Float infinity hex.
	negativeInfinityBits _ '11111111100000000000000000000000'.
	self assert: (Float negativeInfinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeInfinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: negativeInfinityBits readStream base: 2)) hex = Float negativeInfinity hex.! !

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/4/2019 14:16:16'!
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

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'nice 5/30/2006 00:07'!
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

!FloatTest methodsFor: 'tests - 32 bit Single Precision' stamp: 'jmv 4/5/2019 16:24:45'!
test32bitZeros
	| negativeZeroBits zeroBits |
	zeroBits _ '00000000000000000000000000000000'.
	self assert: (Float zero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = zeroBits.
	self assertIsPositiveZero: (Float fromIEEE32Bit: (Integer readFrom: zeroBits readStream base: 2)).
	negativeZeroBits _ '10000000000000000000000000000000'.
	self assert: (Float negativeZero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeZeroBits.
	self assertIsNegativeZero: (Float fromIEEE32Bit: (Integer readFrom: negativeZeroBits readStream base: 2)).! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 4/24/2019 11:48:04'!
assert: aFloatingPointNumber isExactly: otherFloatingPointNumber 

    "Both aFloatingPointNumber and otherFloatingPointNumber must be instances
    of our #classTested. Check that and fail if they aren't."

    self assert: (aFloatingPointNumber class inheritsFrom: Float).
    self assert: (otherFloatingPointNumber class inheritsFrom: Float).
    self assert: aFloatingPointNumber = otherFloatingPointNumber.
    self assert: otherFloatingPointNumber = aFloatingPointNumber.
    self assert: aFloatingPointNumber hex = otherFloatingPointNumber hex! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 2/28/2019 16:13:30'!
assertIsNaNorFail: aFloatOrSymbol
	"Assert that aFloat is NaN or #fail.
	For some BoxedFloat64, primitive 55 (sqrt) faile, unless jitter, where it answers NaN. We consider both values valid."
	self assert: (aFloatOrSymbol = #fail or: [ aFloatOrSymbol isNaN ])! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 10/3/2018 15:43:27'!
assertIsNegativeZero: aFloat
	"Assert that aFloat is Float negativeZero"
	self assert: aFloat hex = Float negativeZero hex! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 10/3/2018 15:43:47'!
assertIsPositiveZero: aFloat
	"Assert that aFloat is Float zero (the positive one)"
	self assert: aFloat hex = 0.0 hex! !

!FloatTest methodsFor: 'helpers' stamp: 'jmv 10/15/2019 16:41:28'!
expectedFailures
	"See https://github.com/OpenSmalltalk/opensmalltalk-vm/issues/383"
	^ (`{'Win32'.  'Mac OS' }` includes: Smalltalk platformName)
		ifTrue: [#(testTimesTwoPowerGradualUnderflow)]
		ifFalse: [#()]! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/27/2019 15:25:35'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/14/2019 14:09:00'!
testPrimArcTan

	self verify: 1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / 4.
	self verify: -1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / -4.
	self verify: 0.0 primTestArcTan isTrigonometricallyEqualTo: 0.
	self verify: Float infinity primTestArcTan isTrigonometricallyEqualTo: Float pi / 2.
	self verify: Float negativeInfinity primTestArcTan isTrigonometricallyEqualTo: Float pi / -2.
	self assert: Float nan primTestArcTan isNaN! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 10/11/2019 20:08:22'!
testPrimDivideBy

	self assert: (1.5 primTestDivideBy: 2.0) = 0.75.

	Smalltalk doMixedArithmetic
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 18:00:27'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/14/2019 13:51:33'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/14/2019 13:44:44'!
testPrimExponent
	self assert: 1.0 primTestExponent = 0.
	self assert: 2.0 predecessor primTestExponent = 0.
	self assert: 2.0 primTestExponent =  1.
	self assert: 4.0 predecessor primTestExponent = 1.
	self assert: 4.0 primTestExponent = 2.
	self assert: Float fminNormalized primTestExponent = -1022.
	self assert: Float fminDenormalized primTestExponent = (-1022-52).! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 17:50:52'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 17:53:17'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 17:46:13'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 17:47:25'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/27/2019 15:26:33'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/25/2019 18:02:13'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/14/2019 13:57:07'!
testPrimSin

	self verify: 0.0 primTestSin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) primTestSin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) primTestSin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) primTestSin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) primTestSin isTrigonometricallyEqualTo: 1.0! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/12/2019 14:31:19'!
testPrimSmallIntegerAsFloat
	self assert: 7 asFloat = 7.0.
	self assert: 0 asFloat = 0.0.
	self assert: 1 asFloat = 1.0.
	self assert: -1 asFloat = -1.0.
	self assert: Float maxExactInteger asFloat = (Float maxExactInteger printString, '.0') asNumber.
	self assert: Float maxExactInteger negated asFloat = (Float maxExactInteger negated printString, '.0') asNumber.
	self assert: SmallInteger maxVal asFloat = (SmallInteger maxVal printString, '.0') asNumber.
	self assert: SmallInteger minVal asFloat = (SmallInteger minVal printString, '.0') asNumber.! !

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/12/2019 14:27:58'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 2/27/2019 15:25:52'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/14/2019 13:53:28'!
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

!FloatTest methodsFor: 'tests - primitives' stamp: 'jmv 3/12/2019 14:39:27'!
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
	"Depends on rounding to nearest Float. This could change depending on SmallInteger maxVal. Currently false in 64 bits Spur image, but true in 32 bits Spur and V3 images."
	SmallInteger maxVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger maxVal asFloat primTestTruncated = SmallInteger maxVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger maxVal asFloat primTestTruncated = #fail ].

	self assert: SmallInteger minVal asFloat successor asTrueFraction truncated class = SmallInteger.
	self assert: SmallInteger minVal asFloat successor primTestTruncated = SmallInteger minVal asFloat successor asTrueFraction truncated.	
	self assert: (SmallInteger minVal-1) asFloat predecessor asTrueFraction truncated class = LargeNegativeInteger.
	self assert: (SmallInteger minVal-1) asFloat predecessor primTestTruncated = # fail.
	"Depends on rounding to nearestFloat. This could change depending on SmallInteger maxVal. Currently true in 64 bits Spur image, and in 32 bits Spur and V3 images."
	SmallInteger minVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger minVal asFloat primTestTruncated = SmallInteger minVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger minVal asFloat primTestTruncated = #fail ].
! !

!FractionTest methodsFor: 'private' stamp: 'jmv 10/11/2011 22:12'!
assert: a classAndValueEquals: b
	self assert: a class = b class.
	self assert: a = b! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/3/2018 15:19:28'!
testDegreeCos2
	"self run: #testDegreeCos"
	
	self shouldnt: [ (4/3) degreeCos] raise: Error.
	self assert: (1/3) degreeCos printString =  '0.9999830768577442'! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/3/2018 15:19:31'!
testDegreeSin2
	"self run: #testDegreeSin"
	
	self shouldnt: [ (4/3) degreeSin] raise: Error.
	self assert: (1/3) degreeSin printString =  '0.005817731354993834'.! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/11/2011 22:27'!
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

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/11/2011 22:12'!
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

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/11/2012 19:56'!
testFloorLog
	self assert: (1/100 floorLog: 10) = -2.
	self assert: (((2 raisedTo: Float emax + 11)/3) floorLog: 10)
		= ((Float emax + 11)*2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) floorLog: 10)
		= ((Float emin - Float precision)*2 log + 3 log) floor description: 'Fraction>>log should not underflow'! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/11/2012 22:27'!
testFloorLogExactness

	1 + (Float fminDenormalized floorLog: 10) to: -1 do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	1/(10 raisedTo: n) asFloat is not exact
	(aFloat log: radix) is not exact

	(1 + (Float fminDenormalized floorLog: 10) to: -1) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."
	! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/13/2011 21:38'!
testInexactRaisedTo
	"
	FractionTest new testInexactRaisedTo
	"
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) raisedTo: 1/3) = 1.0.
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/3) = -1.0! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/13/2011 21:27'!
testInexactSqrt
	"
	FractionTest new testInexactSqrt
	"
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) sqrt = 1.0! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 10:42:49'!
testLn
	self verify: (1/100) ln isWithinOneFloatAwayFrom: -2 * 10 ln.
	self verify: ((2 raisedTo: Float emax + 11) / 3) ln isWithinOneFloatAwayFrom: (Float emax + 11) * 2 ln - 3 ln.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) ln isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 ln + 3 ln! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 10:45:24'!
testLog
	self assert: (1/100) log = -2.
	self verify: ((2 raisedTo: Float emax + 11) / 3) log isWithinOneFloatAwayFrom: (Float emax + 11) * 2 log - 3 log.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) log isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 log + 3 log.! !

!FractionTest methodsFor: 'tests - mathematical functions' stamp: 'nice 10/19/2011 20:48'!
testNthRoot
	self assert: ((-2 raisedTo: 35) / (3 raisedTo: 20) raisedTo: 1/5) equals: (-2 raisedTo: 7) / (3 raisedTo: 4).
	self assert: (1 / (1 << 2000) raisedTo: 1/100) equals: 1 / (1 << 20)! !

!FractionTest methodsFor: 'tests - printing' stamp: 'nice 7/24/2010 18:43'!
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

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:56'!
testIntegerWholeDivision
	
	self assert: 4 / (2/3) classAndValueEquals: 6.
	
	self assert: 4 / (-2/3) classAndValueEquals: -6.
	
	self assert: -4 / (-2/3) classAndValueEquals: 6.
	
	self assert: -4 / (2/3) classAndValueEquals: -6.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:56'!
testIntegerWholeMultiplication
	
	self assert: 4 * (3/2) classAndValueEquals: 6.
	
	self assert: 4 * (-3/2) classAndValueEquals: -6.
	
	self assert: -4 * (-3/2) classAndValueEquals: 6.
	
	self assert: -4 * (3/2) classAndValueEquals: -6.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:56'!
testReciprocal

	self 
		assert: (1/2) reciprocal classAndValueEquals: 2;
		assert: (3/4) reciprocal equals: (4/3);
		assert: (-1/3) reciprocal classAndValueEquals: -3;
		assert: (-3/5) reciprocal equals: (-5/3)! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:56'!
testWholeDifference
	
	self assert: (2/3) - (5/3) classAndValueEquals: -1.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:56'!
testWholeDivision
	
	self assert: (3/2) / (3/4) classAndValueEquals: 2.
	
	self assert: (3/2) / (-3/4) classAndValueEquals: -2.
	
	self assert: (-3/2) / (-3/4) classAndValueEquals: 2.
	
	self assert: (-3/2) / (3/4) classAndValueEquals: -2.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:57'!
testWholeMultiplication
	
	self assert: (3/2) * (4/3) classAndValueEquals: 2.
	
	self assert: (3/2) * (-4/3) classAndValueEquals: -2.
	
	self assert: (-3/2) * (-4/3) classAndValueEquals: 2.
	
	self assert: (-3/2) * (4/3) classAndValueEquals: -2.! !

!FractionTest methodsFor: 'tests - arithmetic' stamp: 'nice 1/22/2012 18:57'!
testWholeSum
	
	self assert: (5/3) + (1/3) classAndValueEquals: 2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 6/3/2011 21:32'!
testCeiling
	self assert: (3 / 2) ceiling = 2.
	self assert: (-3 / 2) ceiling = -1.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 6/3/2011 21:32'!
testFloor
	self assert: (3 / 2) floor = 1.
	self assert: (-3 / 2) floor = -2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 6/3/2011 21:39'!
testRounded
	self assert: (4 / 5) rounded = 1.
	self assert: (6 / 5) rounded = 1.
	self assert: (-4 / 5) rounded = -1.
	self assert: (-6 / 5) rounded = -1.
	
	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded = 2.
	self assert: (-3 / 2) rounded = -2.! !

!FractionTest methodsFor: 'tests - conversions' stamp: 'nice 6/3/2011 21:35'!
testTruncated
	self assert: (3 / 2) truncated = 1.
	self assert: (-3 / 2) truncated = -1.! !

!FractionTest methodsFor: 'tests - invariants' stamp: 'nice 1/22/2012 19:07'!
testThatFractionDenominatorIsPositive
	self assert: (-3 / 2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (-3 / 2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (3 / -2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (-3 / -2) numerator positive description: 'two negative signs are simplified'.
	self assert: (-3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.! !

!FractionTest methodsFor: 'tests - invariants' stamp: 'nice 1/22/2012 19:12'!
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

!FractionTest methodsFor: 'testing' stamp: 'jmv 10/9/2018 09:37:12'!
testHash

	| a fraction1 fraction2 |
	fraction1 _ 1/3.
	fraction2 _ (1/3) + (1e-1000).
	self deny: fraction1 = fraction2 description: 'precondition'.
	self assert: fraction1 asFloat = fraction2 asFloat description: 'precondition'.
	a _ Set new.
	a add: fraction1; add: fraction2.
	self assert: a size = 2! !

!FractionTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:20:34'!
testIsType
	self assert: ((1 / 2100) is: #Fraction).
	self assert: ((1 / 2100) is: #Number).! !

!FractionTest methodsFor: 'tests - rounding' stamp: 'jmv 10/9/2018 16:12:18'!
testRoundHalfToEven
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: (1/2) rounded = 0.
	self assert: (3/2) rounded = 2.
	self assert: (-1/2) rounded = 0.
	self assert: (-3/2) rounded = -2.
	self assert: (((0 to: 5 by: 1/4) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((0 to: -5 by: -1/4) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) ).! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:12'!
testAndSingleBitWithMinusOne
	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i)].! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'nice 1/10/2010 17:46'!
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

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:13'!
testMixedSignDigitLogic
	"Verify that mixed sign logic with large integers works."
	self assert: (-2 bitAnd: 16rFFFFFFFF) = 16rFFFFFFFE! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:12'!
testNBitAndNNegatedEqualsN
	"Verify that (n bitAnd: n negated) = n for single bits"
	| n |
	1 to: 100 do: [:i | n := 1 bitShift: i.
				self assert: (n bitAnd: n negated) = n]! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:12'!
testNNegatedEqualsNComplementedPlusOne
	"Verify that n negated = (n complemented + 1) for single bits"
	| n |
	1 to: 100 do: [:i | n := 1 bitShift: i.
				self assert: n negated = ((n bitXor: -1) + 1)]! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:13'!
testShiftMinusOne1LeftThenRight
	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((-1 bitShift: i) bitShift: i negated) = -1].
! !

!IntegerDigitLogicTest methodsFor: 'tests' stamp: 'hmm 1/7/2002 21:12'!
testShiftOneLeftThenRight
	"Shift 1 bit left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) = 1].
! !

!IntegerTest methodsFor: 'private' stamp: 'jmv 10/11/2011 08:14'!
assert: a classAndValueEquals: b
	self assert: a class = b class.
	self assert: a = b! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:01'!
testBackslashBackslashLnLn

	self assert: -42391158275216203514294433200 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: -42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ -14130386091738734504764811067 = -1.
	self assert: -8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -79766443076872509863360.
	self assert: -8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:00'!
testBackslashBackslashLnLp

	self assert: -42391158275216203514294433200 \\ 14130386091738734504764811067 = 1.
	self assert: -42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: -8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 1.
	self assert: -8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:01'!
testBackslashBackslashLnSn

	self assert: -42391158275216203514294433201 \\ -3 = 0.
	self assert: -42391158275216203514294433202 \\ -3 = -1.
	self assert: -42391158275216203514294433203 \\ -3 = -2.
	self assert: -42391158275216203514294433204 \\ -3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:02'!
testBackslashBackslashLnSp

	self assert: -42391158275216203514294433201 \\ 3 = 0.
	self assert: -42391158275216203514294433202 \\ 3 = 2.
	self assert: -42391158275216203514294433203 \\ 3 = 1.
	self assert: -42391158275216203514294433204 \\ 3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:04'!
testBackslashBackslashLpLn

	self assert: 42391158275216203514294433200 \\ -14130386091738734504764811067 = -1.
	self assert: 42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: 8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -1.
	self assert: 8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:05'!
testBackslashBackslashLpLp

	self assert: 42391158275216203514294433200 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: 42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ 14130386091738734504764811067 = 1.
	self assert: 8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 79766443076872509863360.
	self assert: 8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:06'!
testBackslashBackslashLpSn

	self assert: 42391158275216203514294433201 \\ -3 = 0.
	self assert: 42391158275216203514294433202 \\ -3 = -2.
	self assert: 42391158275216203514294433203 \\ -3 = -1.
	self assert: 42391158275216203514294433204 \\ -3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:07'!
testBackslashBackslashLpSp

	self assert: 42391158275216203514294433201 \\ 3 = 0.
	self assert: 42391158275216203514294433202 \\ 3 = 1.
	self assert: 42391158275216203514294433203 \\ 3 = 2.
	self assert: 42391158275216203514294433204 \\ 3 = 0! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:08'!
testBackslashBackslashSnLn

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: -1 \\ -42391158275216203514294433201 = -1.
	self assert: -14348907 \\ -42391158275216203514294433201 = -14348907! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:09'!
testBackslashBackslashSnLp

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: -1 \\ 42391158275216203514294433201 =  42391158275216203514294433200.
	self assert: -14348907 \\ 42391158275216203514294433201 = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:09'!
testBackslashBackslashSnSn

	self assert: 0 \\ -3 = 0.
	self assert: -1 \\ -3 = -1.
	self assert: -9 \\ -3 = 0.
	self assert: -10 \\ -3 = -1.
	self assert: -11 \\ -3 = -2.
	self assert: -12 \\ -3 = 0.
	self assert: -13 \\ -3 = -1! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:10'!
testBackslashBackslashSnSp

	self assert: 0 \\ 3 = 0.
	self assert: -1 \\ 3 = 2.
	self assert: -9 \\ 3 = 0.
	self assert: -10 \\ 3 = 2.
	self assert: -11 \\ 3 = 1.
	self assert: -12 \\ 3 = 0.
	self assert: -13 \\ 3 = 2! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:11'!
testBackslashBackslashSpLn

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: 1 \\ -42391158275216203514294433201 = -42391158275216203514294433200.
	self assert: 14348907 \\ -42391158275216203514294433201 = -42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:12'!
testBackslashBackslashSpLp

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: 1 \\ 42391158275216203514294433201 = 1.
	self assert: 14348907 \\ 42391158275216203514294433201 = 14348907! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:13'!
testBackslashBackslashSpSn

	self assert: 0 \\ -3 = 0.
	self assert: 1 \\ -3 = -2.
	self assert: 9 \\ -3 = 0.
	self assert: 10 \\ -3 = -2.
	self assert: 11 \\ -3 = -1.
	self assert: 12 \\ -3 = 0.
	self assert: 13 \\ -3 = -2! !

!IntegerTest methodsFor: 'tests - division - \\' stamp: 'sqr 1/25/2014 14:13'!
testBackslashBackslashSpSp

	self assert: 0 \\ 3 = 0.
	self assert: 1 \\ 3 = 1.
	self assert: 9 \\ 3 = 0.
	self assert: 10 \\ 3 = 1.
	self assert: 11 \\ 3 = 2.
	self assert: 12 \\ 3 = 0.
	self assert: 13 \\ 3 = 1! !

!IntegerTest methodsFor: 'tests - benchmarks' stamp: 'sd 6/5/2005 08:37'!
testBenchFib

	self assert: (0 benchFib = 1).
	self assert: (1 benchFib = 1).
	self assert: (2 benchFib = 3).
	! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 8/21/2016 20:02:52'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 8/21/2016 20:03:04'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/13/2011 21:46'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/11/2011 22:09'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/11/2012 19:53'!
testFloorLog
	self assert: (100 floorLog: 10) = 2.
	self assert: (((2 raisedTo: Float emax + 3) floorLog: 10) = (2 log*(Float emax + 3)) floor) description: 'Integer>>floorLog: should not overflow'! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 12/11/2012 22:30'!
testFloorLogExactness

	1 to: (Float fmax floorLog: 10) do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	(10 raisedTo: n) asFloat is not exact for n > 22
	(aFloat log: radix) is not exact

	(1 to: (Float fmax floorLog: 10)) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 10:47:37'!
testLn
	self verify: 100 ln isWithinOneFloatAwayFrom: 10 ln*2.
	self verify: (2 raisedTo: Float emax + 3) ln isWithinOneFloatAwayFrom: 2 ln * (Float emax + 3)! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 10:48:49'!
testLog
	self assert: 100 log = 2.
	self verify: (2 raisedTo: Float emax + 3) log isWithinOneFloatAwayFrom: 2 log * (Float emax + 3)! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/19/2011 22:48'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 10/13/2011 09:09'!
testNthRootErrorConditions
	"
	IntegerTest new testExactRaisedToErrorConditions
	"

	self should: [ -2 nthRoot: 1/4 ] raise: ArithmeticError.
	self should: [ -2 nthRoot: 1.24 ] raise: ArithmeticError.! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 3/15/2014 14:50'!
testNthRootExactness
	| inexactRoots largeRaisedTo6 |
	largeRaisedTo6 := (2 to: 100) collect: [:k | (k raisedTo: 11) raisedTo: 6].
	inexactRoots := largeRaisedTo6 reject: [:e | (e nthRoot: 6) isInteger].
	self assert: inexactRoots isEmpty description: 'Failed to find the exact 6th root of these numbers'! !

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'nice 10/19/2011 21:03'!
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

!IntegerTest methodsFor: 'tests - mathematical functions' stamp: 'ul 11/25/2009 03:01'!
testSqrtFloor

	#(-1234567890123 -10 -5 -1) do: [ :each |
		self should: [ each sqrtFloor ] raise: Error ].
	#(
		0 1 2 3 4 5 10 16 30 160479924 386234481 501619156 524723498 580855366 766098594 834165249 1020363860 1042083924 1049218924
		1459774772895569 3050005981408238 4856589481837079 5650488387708463 7831037396100244) do: [ :each |
			self assert: each asFloat sqrt floor = each sqrtFloor ]
		! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 12/27/2009 03:11'!
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

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'sd 6/5/2005 08:43'!
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

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 10/3/2018 18:03:31'!
testBitReversal
	{ 0. 1. SmallInteger maxVal-1. SmallInteger maxVal. SmallInteger maxVal+1. (2 raisedTo: 64)-1091. (2 raisedTo: 64)-1090. (2 raisedTo: 64)-1 } do: 
		[ : fixture |
		| printedThenReversed reversedThenPrinted |
		printedThenReversed := (fixture printStringBase: 2 length: 64 padded: true) reversed.
		reversedThenPrinted := (fixture bitReverse: 64) printStringBase: 2 length: 64 padded: true.
		self assert: printedThenReversed = reversedThenPrinted ]! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 6/24/2020 11:23:19'!
testBitReversalCorrectClass
	"This test was added due to a bug!!
	The problem only happened in 64 bit systems running with jitted Cog VM."

	| a b |
	a _ 16r944F245FDBB2B06.
	10 timesRepeat: [
		b _ (a bitReverse: 64) bitReverse: 64.
		self assert: a class = b class ]! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 7/8/2008 02:47'!
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

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 7/8/2008 02:44'!
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

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'jmv 12/10/2018 18:36:38'!
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

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'nice 1/26/2008 02:22'!
testTwoComplementBitLogicWithCarry
	"This is non regression test for http://bugs.squeak.org/view.php?id=6874"
	
	"By property of two complement, following operation is:
	...111110000 this is -16
	...111101111 this is -16-1
	...111100000 this is -32, the result of bitAnd: on two complement
	
	This test used to fail with n=31 39 47.... because of bug 6874"
	
	self assert: ((2 to: 80) allSatisfy: [:n | ((2 raisedTo: n) negated bitAnd: (2 raisedTo: n) negated - 1) = (2 raisedTo: n + 1) negated]).! !

!IntegerTest methodsFor: 'tests - bitLogic' stamp: 'sd 6/5/2005 08:44'!
testTwoComplementRightShift
	"self run: #testTwoComplementRightShift"

	| large small |
	small := 2 << 16.
	large := 2 << 32.	
	self assert: ((small negated bitShift: -1) ~= ((small + 1) negated bitShift: -1)
		== ((large negated bitShift: -1) ~= ((large + 1) negated bitShift: -1))).
		
     self assert: ((small bitShift: -1) ~= (small + 1 bitShift: -1)
		== ((large bitShift: -1) ~= (large + 1 bitShift: -1))).! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'mga 5/11/2006 15:41'!
testCrossSumBase
	"self run: #testCrossSumBase"

	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 10]) asArray = 
		#(2 10 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10 2)).
	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 2]) asArray = 
		#(2 3 2 2 1 4 3 3 2 3 2 2 1 3 2 2 1 2 1 1 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2)).
	self should: [10 crossSumBase: 1] raise: AssertionFailure! !

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 1/9/2013 02:11'!
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

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 1/16/2013 18:38'!
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

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 1/9/2013 02:11'!
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

!IntegerTest methodsFor: 'testing - arithmetic' stamp: 'nice 6/4/2011 20:46'!
testRaisedToModulo
	#(301 2047) do: [:m |
		1 to: m - 1 by: (m // 30) do: [:x |
			11 to: m - 1 by: (m // 40) do: [:y |
				self assert: (x raisedTo: y) \\ m = (x raisedTo: y modulo: m)]]].
	self assert: (8951195 raisedTo: 7742931 modulo: 15485863) = 15485862.! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 4/21/2003 16:17'!
testEven
	
	self deny: (1073741825 even).
	self assert: (1073741824  even).
	! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'jpb 8/2/2019 23:55:59'!
testIsInteger
  self assert: 0 isInteger.
  self assert: (0 is: #Integer).
	self assert: (1 is: #Integer).
	self assert: (1 is: #Number).
	
	self assert: (10000 is: #Integer).
	self assert: (10000 is: #Number).! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 4/15/2003 20:40'!
testIsPowerOfTwo

	self assert: (0 isPowerOfTwo).
	self assert: (1 isPowerOfTwo).
	self assert: (2 isPowerOfTwo).
	self deny:  (3 isPowerOfTwo).
	self assert: (4 isPowerOfTwo).
	! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'nice 1/25/2008 22:51'!
testIsPowerOfTwoM6873
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6873"

	self deny: ((1 to: 80) anySatisfy: [:n | (2 raisedTo: n) negated isPowerOfTwo])
		description: 'A negative integer cannot be a power of two'.! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'ul 11/25/2009 02:51'!
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

!IntegerTest methodsFor: 'tests - basic' stamp: 'ul 11/25/2009 02:49'!
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

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 2/12/2006 14:36'!
testLargePrimesUpTo

	| nn | 
	nn := (2 raisedTo: 17) - 1. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
	
	
! !

!IntegerTest methodsFor: 'tests - basic' stamp: 'md 2/12/2006 14:36'!
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

!IntegerTest methodsFor: 'tests - basic' stamp: 'eem 7/16/2014 15:29'!
testRange
	self assert: SmallInteger maxVal class equals: SmallInteger.
	self assert: (SmallInteger maxVal + 1) class equals: LargePositiveInteger.
	self assert: SmallInteger minVal class equals: SmallInteger.
	self assert: (SmallInteger minVal - 1) class equals: LargeNegativeInteger! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'sd 6/5/2005 08:46'!
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

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 9/1/2010 13:56'!
testIntegerReadFrom
	self assert: (Integer readFrom: '123' readStream base: 10) = 123.
	self assert: (Integer readFrom: '-123' readStream base: 10) = -123.
	self should: [Integer readFrom: 'abc' readStream base: 10] raise: Error.
	self should: [Integer readFrom: 'D12' readStream base: 10] raise: Error.
	self assert: (Integer readFrom: '1two3' readStream base: 10) = 1.
! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'md 3/25/2003 23:14'!
testNew
	self should: [Integer new] raise: TestResult error. ! !

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 9/1/2010 13:26'!
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

!IntegerTest methodsFor: 'tests - instance creation' stamp: 'jmv 9/1/2010 13:45'!
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

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:53'!
testDivLnLn

	self assert: (-42391158275216203514294433200 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433201 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 div: -14130386091738734504764811067) = 4.
	self assert: (-8727963568087712425891397479476727340041448 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: -79766443076872509863361) = 109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:53'!
testDivLnLp

	self assert: (-42391158275216203514294433200 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433201 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 div: 14130386091738734504764811067) = -4.
	self assert: (-8727963568087712425891397479476727340041448 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: 79766443076872509863361) = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:54'!
testDivLnSn

	self assert: (-42391158275216203514294433201 div: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: -3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivLnSp

	self assert: (-42391158275216203514294433201 div: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: 3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:54'!
testDivLpLn

	self assert: (42391158275216203514294433200 div: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 div: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 div: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 div: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: -79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivLpLp

	self assert: (42391158275216203514294433200 div: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 div: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 div: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 div: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: 79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:55'!
testDivLpSn

	self assert: (42391158275216203514294433201 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: -3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivLpSp

	self assert: (42391158275216203514294433201 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: 3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:55'!
testDivSnLn

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (-1 div: -42391158275216203514294433201) = 1.
	self assert: (-14348907 div: -42391158275216203514294433201) = 1! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivSnLp

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (-1 div: 42391158275216203514294433201) = -1.
	self assert: (-14348907 div: 42391158275216203514294433201) = -1! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:56'!
testDivSnSn

	self assert: (0 div: -3) = 0.
	self assert: (-1 div: -3) = 1.
	self assert: (-9 div: -3) = 3.
	self assert: (-10 div: -3) = 4.
	self assert: (-11 div: -3) = 4.
	self assert: (-12 div: -3) = 4.
	self assert: (-13 div: -3) = 5! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivSnSp

	self assert: (0 div: 3) = 0.
	self assert: (-1 div: 3) = -1.
	self assert: (-9 div: 3) = -3.
	self assert: (-10 div: 3) = -4.
	self assert: (-11 div: 3) = -4.
	self assert: (-12 div: 3) = -4.
	self assert: (-13 div: 3) = -5! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:55'!
testDivSpLn

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (1 div: -42391158275216203514294433201) = 0.
	self assert: (14348907 div: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivSpLp

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (1 div: 42391158275216203514294433201) = 0.
	self assert: (14348907 div: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:54'!
testDivSpSn

	self assert: (0 div: -3) = 0.
	self assert: (1 div: -3) = 0.
	self assert: (9 div: -3) = -3.
	self assert: (10 div: -3) = -3.
	self assert: (11 div: -3) = -3.
	self assert: (12 div: -3) = -4.
	self assert: (13 div: -3) = -4! !

!IntegerTest methodsFor: 'tests - division - div:' stamp: 'sqr 1/25/2014 14:36'!
testDivSpSp

	self assert: (0 div: 3) = 0.
	self assert: (1 div: 3) = 0.
	self assert: (9 div: 3) = 3.
	self assert: (10 div: 3) = 3.
	self assert: (11 div: 3) = 3.
	self assert: (12 div: 3) = 4.
	self assert: (13 div: 3) = 4! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 1/9/2014 23:39'!
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

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 1/9/2014 22:54'!
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

!IntegerTest methodsFor: 'tests - printing' stamp: 'nice 2/15/2008 22:23'!
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

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 1/9/2014 23:00'!
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

!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 10/3/2018 18:05:41'!
testPrintOnBaseShowRadix

	self assert: (123 printStringRadix: 10) = '10r123'.
	self assert: (123 printStringRadix: 8) = '8r173'.! !

!IntegerTest methodsFor: 'tests - printing' stamp: 'nice 2/15/2008 22:31'!
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

!IntegerTest methodsFor: 'tests - printing' stamp: 'laza 3/30/2004 09:23'!
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

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:57'!
testModLnLn

	self assert: (-42391158275216203514294433200 mod: -14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModLnLp

	self assert: (-42391158275216203514294433200 mod: 14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 79766443076872509863360! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:05'!
testModLnSn

	self assert: (-42391158275216203514294433201 mod: -3) = 0.
	self assert: (-42391158275216203514294433202 mod: -3) = 2.
	self assert: (-42391158275216203514294433203 mod: -3) = 1.
	self assert: (-42391158275216203514294433204 mod: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModLnSp

	self assert: (-42391158275216203514294433201 mod: 3) = 0.
	self assert: (-42391158275216203514294433202 mod: 3) = 2.
	self assert: (-42391158275216203514294433203 mod: 3) = 1.
	self assert: (-42391158275216203514294433204 mod: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:05'!
testModLpLn

	self assert: (42391158275216203514294433200 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModLpLp

	self assert: (42391158275216203514294433200 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:04'!
testModLpSn

	self assert: (42391158275216203514294433201 mod: -3) = 0.
	self assert: (42391158275216203514294433202 mod: -3) = 1.
	self assert: (42391158275216203514294433203 mod: -3) = 2.
	self assert: (42391158275216203514294433204 mod: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModLpSp

	self assert: (42391158275216203514294433201 mod: 3) = 0.
	self assert: (42391158275216203514294433202 mod: 3) = 1.
	self assert: (42391158275216203514294433203 mod: 3) = 2.
	self assert: (42391158275216203514294433204 mod: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:04'!
testModSnLn

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (-1 mod: -42391158275216203514294433201) = 42391158275216203514294433200.
	self assert: (-14348907 mod: -42391158275216203514294433201) = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModSnLp

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (-1 mod: 42391158275216203514294433201) =  42391158275216203514294433200.
	self assert: (-14348907 mod: 42391158275216203514294433201) = 42391158275216203514280084294! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:00'!
testModSnSn

	self assert: (0 mod: -3) = 0.
	self assert: (-1 mod: -3) = 2.
	self assert: (-9 mod: -3) = 0.
	self assert: (-10 mod: -3) = 2.
	self assert: (-11 mod: -3) = 1.
	self assert: (-12 mod: -3) = 0.
	self assert: (-13 mod: -3) = 2! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:59'!
testModSnSp

	self assert: (0 mod: 3) = 0.
	self assert: (-1 mod: 3) = 2.
	self assert: (-9 mod: 3) = 0.
	self assert: (-10 mod: 3) = 2.
	self assert: (-11 mod: 3) = 1.
	self assert: (-12 mod: 3) = 0.
	self assert: (-13 mod: 3) = 2! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 15:05'!
testModSpLn

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (1 mod: -42391158275216203514294433201) = 1.
	self assert: (14348907 mod: -42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModSpLp

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (1 mod: 42391158275216203514294433201) = 1.
	self assert: (14348907 mod: 42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:58'!
testModSpSn

	self assert: (0 mod: -3) = 0.
	self assert: (1 mod: -3) = 1.
	self assert: (9 mod: -3) = 0.
	self assert: (10 mod: -3) = 1.
	self assert: (11 mod: -3) = 2.
	self assert: (12 mod: -3) = 0.
	self assert: (13 mod: -3) = 1! !

!IntegerTest methodsFor: 'tests - division - mod:' stamp: 'sqr 1/25/2014 14:35'!
testModSpSp

	self assert: (0 mod: 3) = 0.
	self assert: (1 mod: 3) = 1.
	self assert: (9 mod: 3) = 0.
	self assert: (10 mod: 3) = 1.
	self assert: (11 mod: 3) = 2.
	self assert: (12 mod: 3) = 0.
	self assert: (13 mod: 3) = 1! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 10/15/2016 10:58:02'!
testNthRootExactnessForHugeValue
	"
	self new testNthRootExactnessForHugeValue
	"
        self assert: ((10 raisedTo: 600) nthRoot: 300) = 100.
        self assert: ((10 raisedTo: 600) nthRoot: 300) class == SmallInteger.
        self assert: ((10 raisedTo: 600) + 1 nthRoot: 300) = 100.0.
        self assert: (((10 raisedTo: 600) + 1 nthRoot: 300) isKindOf: Float)! !

!IntegerTest methodsFor: 'mathematical functions' stamp: 'jmv 10/15/2016 10:57:28'!
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

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:36'!
testQuoLnLn

	self assert: (-42391158275216203514294433200 quo: -14130386091738734504764811067) = 2.
	self assert: (-42391158275216203514294433201 quo: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 quo: -14130386091738734504764811067) = 3.
	self assert: (-8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = 109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:51'!
testQuoLnLp

	self assert: (-42391158275216203514294433200 quo: 14130386091738734504764811067) = -2.
	self assert: (-42391158275216203514294433201 quo: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 quo: 14130386091738734504764811067) = -3.
	self assert: (-8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = -109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoLnSn

	self assert: (-42391158275216203514294433201 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: -3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:51'!
testQuoLnSp

	self assert: (-42391158275216203514294433201 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: 3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:52'!
testQuoLpLn

	self assert: (42391158275216203514294433200 quo: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 quo: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 quo: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = -109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoLpLp

	self assert: (42391158275216203514294433200 quo: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 quo: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 quo: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:50'!
testQuoLpSn

	self assert: (42391158275216203514294433201 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: -3) = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoLpSp

	self assert: (42391158275216203514294433201 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: 3) = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoSnLn

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (-1 quo: -42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:50'!
testQuoSnLp

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (-1 quo: 42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoSnSn

	self assert: (0 quo: -3) = 0.
	self assert: (-1 quo: -3) = 0.
	self assert: (-9 quo: -3) = 3.
	self assert: (-10 quo: -3) = 3.
	self assert: (-11 quo: -3) = 3.
	self assert: (-12 quo: -3) = 4.
	self assert: (-13 quo: -3) = 4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:51'!
testQuoSnSp

	self assert: (0 quo: 3) = 0.
	self assert: (-1 quo: 3) = 0.
	self assert: (-9 quo: 3) = -3.
	self assert: (-10 quo: 3) = -3.
	self assert: (-11 quo: 3) = -3.
	self assert: (-12 quo: 3) = -4.
	self assert: (-13 quo: 3) = -4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:52'!
testQuoSpLn

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (1 quo: -42391158275216203514294433201) = 0.
	self assert: (14348907 quo: -42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoSpLp

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (1 quo: 42391158275216203514294433201) = 0.
	self assert: (14348907 quo: 42391158275216203514294433201) = 0! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:50'!
testQuoSpSn

	self assert: (0 quo: -3) = 0.
	self assert: (1 quo: -3) = 0.
	self assert: (9 quo: -3) = -3.
	self assert: (10 quo: -3) = -3.
	self assert: (11 quo: -3) = -3.
	self assert: (12 quo: -3) = -4.
	self assert: (13 quo: -3) = -4! !

!IntegerTest methodsFor: 'tests - division - quo:' stamp: 'sqr 1/25/2014 14:33'!
testQuoSpSp

	self assert: (0 quo: 3) = 0.
	self assert: (1 quo: 3) = 0.
	self assert: (9 quo: 3) = 3.
	self assert: (10 quo: 3) = 3.
	self assert: (11 quo: 3) = 3.
	self assert: (12 quo: 3) = 4.
	self assert: (13 quo: 3) = 4! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemLnLn

	self assert: (-42391158275216203514294433200 rem: -14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: -14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:47'!
testRemLnLp

	self assert: (-42391158275216203514294433200 rem: 14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: 14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemLnSn

	self assert: (-42391158275216203514294433201 rem: -3) = 0.
	self assert: (-42391158275216203514294433202 rem: -3) = -1.
	self assert: (-42391158275216203514294433203 rem: -3) = -2.
	self assert: (-42391158275216203514294433204 rem: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:49'!
testRemLnSp

	self assert: (-42391158275216203514294433201 rem: 3) = 0.
	self assert: (-42391158275216203514294433202 rem: 3) = -1.
	self assert: (-42391158275216203514294433203 rem: 3) = -2.
	self assert: (-42391158275216203514294433204 rem: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:48'!
testRemLpLn

	self assert: (42391158275216203514294433200 rem: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemLpLp

	self assert: (42391158275216203514294433200 rem: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:49'!
testRemLpSn

	self assert: (42391158275216203514294433201 rem: -3) = 0.
	self assert: (42391158275216203514294433202 rem: -3) = 1.
	self assert: (42391158275216203514294433203 rem: -3) = 2.
	self assert: (42391158275216203514294433204 rem: -3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemLpSp

	self assert: (42391158275216203514294433201 rem: 3) = 0.
	self assert: (42391158275216203514294433202 rem: 3) = 1.
	self assert: (42391158275216203514294433203 rem: 3) = 2.
	self assert: (42391158275216203514294433204 rem: 3) = 0! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemSnLn

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (-1 rem: -42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: -42391158275216203514294433201) = -14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:47'!
testRemSnLp

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (-1 rem: 42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: 42391158275216203514294433201) = -14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemSnSn

	self assert: (0 rem: -3) = 0.
	self assert: (-1 rem: -3) = -1.
	self assert: (-9 rem: -3) = 0.
	self assert: (-10 rem: -3) = -1.
	self assert: (-11 rem: -3) = -2.
	self assert: (-12 rem: -3) = 0.
	self assert: (-13 rem: -3) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:37'!
testRemSnSp

	self assert: (0 rem: 3) = 0.
	self assert: (-1 rem: 3) = -1.
	self assert: (-9 rem: 3) = 0.
	self assert: (-10 rem: 3) = -1.
	self assert: (-11 rem: 3) = -2.
	self assert: (-12 rem: 3) = 0.
	self assert: (-13 rem: 3) = -1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:38'!
testRemSpLn

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (1 rem: -42391158275216203514294433201) = 1.
	self assert: (14348907 rem: -42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemSpLp

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (1 rem: 42391158275216203514294433201) = 1.
	self assert: (14348907 rem: 42391158275216203514294433201) = 14348907! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:48'!
testRemSpSn

	self assert: (0 rem: -3) = 0.
	self assert: (1 rem: -3) = 1.
	self assert: (9 rem: -3) = 0.
	self assert: (10 rem: -3) = 1.
	self assert: (11 rem: -3) = 2.
	self assert: (12 rem: -3) = 0.
	self assert: (13 rem: -3) = 1! !

!IntegerTest methodsFor: 'tests - division - rem:' stamp: 'sqr 1/25/2014 14:35'!
testRemSpSp

	self assert: (0 rem: 3) = 0.
	self assert: (1 rem: 3) = 1.
	self assert: (9 rem: 3) = 0.
	self assert: (10 rem: 3) = 1.
	self assert: (11 rem: 3) = 2.
	self assert: (12 rem: 3) = 0.
	self assert: (13 rem: 3) = 1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:56'!
testSlashSlashLnLn

	self assert: -42391158275216203514294433200 // -14130386091738734504764811067 = 2.
	self assert: -42391158275216203514294433201 // -14130386091738734504764811067 = 3.
	self assert: -42391158275216203514294433202 // -14130386091738734504764811067 = 3.
	self assert: -8727963568087712425891397479476727340041448 // -79766443076872509863361 = 109418989131512359208.
	self assert: -8727963568087712425891397479476727340041449 // -79766443076872509863361 = 109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // -79766443076872509863361 = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:47'!
testSlashSlashLnLp

	self assert: -42391158275216203514294433200 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433201 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433202 // 14130386091738734504764811067 = -4.
	self assert: -8727963568087712425891397479476727340041448 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041449 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // 79766443076872509863361 = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:54'!
testSlashSlashLnSn

	self assert: -42391158275216203514294433201 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433203 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433204 // -3 = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:45'!
testSlashSlashLnSp

	self assert: -42391158275216203514294433201 // 3 = -14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433203 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433204 // 3 = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:54'!
testSlashSlashLpLn

	self assert: 42391158275216203514294433200 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433201 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433202 // -14130386091738734504764811067 = -4.
	self assert: 8727963568087712425891397479476727340041448 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041449 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // -79766443076872509863361 = -109418989131512359210! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:52'!
testSlashSlashLpSn

	self assert: 42391158275216203514294433201 // -3 = -14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433203 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433204 // -3 = -14130386091738734504764811068! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:52'!
testSlashSlashSnLn

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: -1 // -42391158275216203514294433201 = 0.
	self assert: -14348907 // -42391158275216203514294433201 = 0! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:44'!
testSlashSlashSnLp

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: -1 // 42391158275216203514294433201 = -1.
	self assert: -14348907 // 42391158275216203514294433201 = -1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:51'!
testSlashSlashSnSn

	self assert: 0 // -3 = 0.
	self assert: -1 // -3 = 0.
	self assert: -9 // -3 = 3.
	self assert: -10 // -3 = 3.
	self assert: -11 // -3 = 3.
	self assert: -12 // -3 = 4.
	self assert: -13 // -3 = 4! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:42'!
testSlashSlashSnSp

	self assert: 0 // 3 = 0.
	self assert: -1 // 3 = -1.
	self assert: -9 // 3 = -3.
	self assert: -10 // 3 = -4.
	self assert: -11 // 3 = -4.
	self assert: -12 // 3 = -4.
	self assert: -13 // 3 = -5! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:51'!
testSlashSlashSpLn

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: 1 // -42391158275216203514294433201 = -1.
	self assert: 14348907 // -42391158275216203514294433201 = -1! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:44'!
testSlashSlashSpLp

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: 1 // 42391158275216203514294433201 = 0.
	self assert: 14348907 // 42391158275216203514294433201 = 0! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:50'!
testSlashSlashSpSn

	self assert: 0 // -3 = 0.
	self assert: 1 // -3 = -1.
	self assert: 9 // -3 = -3.
	self assert: 10 // -3 = -4.
	self assert: 11 // -3 = -4.
	self assert: 12 // -3 = -4.
	self assert: 13 // -3 = -5! !

!IntegerTest methodsFor: 'tests - division - //' stamp: 'sqr 1/25/2014 13:42'!
testSlashSlashSpSp

	self assert: 0 // 3 = 0.
	self assert: 1 // 3 = 0.
	self assert: 9 // 3 = 3.
	self assert: 10 // 3 = 3.
	self assert: 11 // 3 = 3.
	self assert: 12 // 3 = 4.
	self assert: 13 // 3 = 4! !

!IntegerTest methodsFor: 'tests - division' stamp: 'sqr 1/25/2014 13:38'!
testSlashSlashLpLp

	self assert: 42391158275216203514294433200 // 14130386091738734504764811067 = 2.
	self assert: 42391158275216203514294433201 // 14130386091738734504764811067 = 3.
	self assert: 42391158275216203514294433202 // 14130386091738734504764811067 = 3.
	self assert: 8727963568087712425891397479476727340041448 // 79766443076872509863361 = 109418989131512359208.
	self assert: 8727963568087712425891397479476727340041449 // 79766443076872509863361 = 109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // 79766443076872509863361 = 109418989131512359209! !

!IntegerTest methodsFor: 'tests - division' stamp: 'sqr 1/25/2014 13:33'!
testSlashSlashLpSp

	self assert: 42391158275216203514294433201 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433203 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433204 // 3 = 14130386091738734504764811068! !

!IntegerTest methodsFor: 'as yet unclassified' stamp: 'jmv 9/10/2018 10:28:17'!
test01readFromWithEmptyStringRaisesAnError
	|input|
	
	input := ReadStream on: ''.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'! !

!IntegerTest methodsFor: 'as yet unclassified' stamp: 'jmv 9/10/2018 10:28:20'!
test02readFromWithInvalidStringRaisesAnError
	| input |
	
	input := ReadStream on: 'hello'.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 6/27/2019 10:23:39'!
testGCDAndLCM
	| a b c |
	a _ (-20 to: 20) atRandom.
	b _ (-20 to: 20) atRandom.
	c _ (-20 to: 20) atRandom.
	self assert: (a gcd: b) * (a lcm: b) = (a*b) abs.
	self assert: (a gcd: (b lcm: c)) = ((a gcd: b) lcm: (a gcd: c)).
	self assert: (a lcm: (b gcd: c)) = ((a lcm: b) gcd: (a lcm: c))! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 6/27/2019 10:25:17'!
testGCDExample
	self assert: (120 gcd: 70) = 10! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 6/27/2019 10:38:26'!
testGCDProperties
	| a b c |
	a _ (-20 to: 20) atRandom.
	b _ (-20 to: 20) atRandom.
	c _ (-20 to: 20) atRandom.
	self assert: a \\ (a gcd: b) = 0. "divides a"
	self assert: b \\ (a gcd: b) = 0. "divides b"
	self assert: (a gcd: b) = (a gcd: b) abs. "it is normalized to be >= 0"
	self assert: (a gcd: b) = (b gcd: a). "commutative"
	self assert: (a gcd: (b gcd: c)) = ((a gcd: b) gcd: c). "associative"
	self assert: ((a gcd: 0) = a abs or: [a = 0]).
	self assert: (0 gcd: 0) = 0.
	self assert: (a + (b*c) gcd: b) = (a gcd: b).
	self assert: (a*c gcd: b*c) = ((a gcd: b) * c abs)! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 6/27/2019 10:50:25'!
testLCMExample
	self assert: (120 lcm: 70) = 840! !

!IntegerTest methodsFor: 'gcd and lcm' stamp: 'len 6/27/2019 10:48:22'!
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

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'jmv 8/21/2016 20:04:13'!
testCompactClassIndex
	Smalltalk isSpur ifFalse: [
		self assert: LargeNegativeInteger indexIfCompact = 4 ]! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'ul 11/15/2010 11:51'!
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

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:40'!
testDigitAt

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lni digitAt: i.
		self assert: i equals: digit]
! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 22:25'!
testDigitAtPut

	| lni |
	lni := LargeNegativeInteger new: 20.
	1 to: 20 do: [:i | lni digitAt: i put: i].
	self assert: -114605103402541699037609980192546360895434064385equals: lni
! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:39'!
testDigitLength

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	self assert: 20 equals: lni digitLength
! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'ul 11/15/2010 11:52'!
testEmptyTemplate
	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargeNegativeInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '-0'.
	self assert: i normalize = 0.
	self assert: (i normalize isMemberOf: SmallInteger)! !

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'nice 8/31/2012 23:00'!
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

!LargeNegativeIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 22:26'!
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
		self assert: e equals: digit].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lni20 digitAt: e.
		replacementDigit := lni7 digitAt: e - 4.
		self assert: replacementDigit equals: digit]
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 9/1/2010 13:47'!
testBitShift

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) = i]].! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 8/21/2016 20:04:20'!
testCompactClassIndex
	Smalltalk isSpur ifFalse: [
		self assert: LargePositiveInteger indexIfCompact = 5 ]! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'ul 11/15/2010 11:52'!
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

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:40'!
testDigitAt

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lpi digitAt: i.
		self assert: i equals: digit]
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:35'!
testDigitAtPut

	| lpi |
	lpi := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi digitAt: i put: i].
	self assert: 114605103402541699037609980192546360895434064385equals: lpi
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:39'!
testDigitLength

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	self assert: 20 equals: lpi digitLength
! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'ul 11/15/2010 11:52'!
testEmptyTemplate

	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '0'.
	self assert: i normalize = 0.
	self assert: (i normalize isMemberOf: SmallInteger)! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'nice 3/21/2014 18:38'!
testLargeSqrtFloor
	"This test fails if a careless implementation naivly factors out the power of two (remove the trailing zeroes up to lowBit).
	This was the case in a previous Squeak 4.x implementation."

	| large root |
	large := (SmallInteger maxVal << 100 + 1) << 100.
	root := large sqrtFloor.
	self assert: root squared <= large.
	self assert: (root+1) squared > large.! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'sd 6/5/2005 08:52'!
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

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'jmv 9/1/2010 13:48'!
testNormalize
	"self run: #testNormalize"
	"Check normalization and conversion to/from SmallInts"

	self assert: ((SmallInteger maxVal + 1 - 1) = SmallInteger maxVal).
	self assert: (SmallInteger maxVal + 3 - 6) = (SmallInteger maxVal-3).
	self should: ((SmallInteger minVal - 1 + 1) = SmallInteger minVal).
	self assert: (SmallInteger minVal - 3 + 6) = (SmallInteger minVal+3).! !

!LargePositiveIntegerTest methodsFor: 'tests' stamp: 'dtl 7/22/2014 21:59'!
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
		self assert: e equals: digit].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lpi20 digitAt: e.
		replacementDigit := lpi7 digitAt: e - 4.
		self assert: replacementDigit equals: digit]
! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 5/26/2009 21:57'!
testFractionPart

	self 
		assert: 2 fractionPart = 0;
		assert: (1/2) fractionPart = (1/2);
		assert: (4/3) fractionPart = (1/3);
		assert: 2.0 fractionPart = 0.0;
		assert: 0.5 fractionPart = 0.5;
		assert: 2.5 fractionPart = 0.5
! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 11/21/2018 17:38:59'!
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

!NumberTest methodsFor: 'tests' stamp: 'jmv 11/21/2018 17:39:36'!
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

!NumberTest methodsFor: 'tests' stamp: 'jmv 11/21/2018 17:39:51'!
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

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 5/26/2009 21:55'!
testIntegerPart

	self 
		assert: 2 integerPart = 2;
		assert: (1/2) integerPart = 0;
		assert: (4/3) integerPart = 1;
		assert: 2.0 integerPart = 2.0;
		assert: 0.5 integerPart = 0.0;
		assert: 2.5 integerPart = 2.0
! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 5/23/2009 20:49'!
testOne

	self 
		assert: Integer one = 1;
		assert: Float one = 1.0;
		assert: Fraction one = 1! !

!NumberTest methodsFor: 'tests' stamp: 'jmv 1/9/2014 21:23'!
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

!NumberTest methodsFor: 'tests' stamp: 'jmv 12/21/2018 12:00:50'!
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

!NumberTest methodsFor: 'tests' stamp: 'jmv 1/9/2014 21:22'!
testPrintShowingDecimalPlaces3
	"This problem were reported at http://bugs.squeak.org/view.php?id=7028
	unfortunate inversion of left / right padding"
	self assert: (String streamContents: [ :s | 1.009 printOn: s fractionDigits: 3]) = '1.009'.
	self assert: (String streamContents: [ :s | 35.900 printOn: s fractionDigits: 3]) = '35.900'.
	self assert: (String streamContents: [ :s | -0.097 printOn: s fractionDigits: 3]) = '-0.097'.! !

!NumberTest methodsFor: 'tests' stamp: 'nice 12/6/2007 21:24'!
testRaisedTo
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 5/25/2009 16:41'!
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

!NumberTest methodsFor: 'tests' stamp: 'jmv 4/5/2019 17:38:36'!
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

!NumberTest methodsFor: 'tests' stamp: 'sd 6/5/2005 08:56'!
testReadFrom
	
	self assert: 1.0e-14	= (Number readFrom: '1.0e-14').
	self assert: 2r1e26	= (Number readFrom: '2r1e26').! !

!NumberTest methodsFor: 'tests' stamp: 'GabrielOmarCotelli 5/23/2009 19:26'!
testReciprocal

	self 
		assert: 1 reciprocal = 1;
		assert: 2 reciprocal = (1/2);
		assert: -1 reciprocal = -1;
		assert: -3 reciprocal = (-1/3).
		
	self should: [ 0 reciprocal ] raise: ZeroDivide! !

!NumberTest methodsFor: 'tests' stamp: 'sqr 6/26/2019 09:46:32'!
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

!NumberTest methodsFor: 'test' stamp: 'GC 9/8/2018 14:51:43'!
test01readFromWhenTheInitilValueIsNaNTheResultIsNaN

	|initialValue nan result |
	
	nan := Float nan.
	initialValue := 'NaN'.
	result := self subject: initialValue.
	
	self assert: result is: nan! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/8/2018 15:07:25'!
test02readFromWhenTheInitilValueIsInfinityTheResultIsInfinity
	
	self assertThat: 'Infinity' isEqualTo: Float infinity! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/8/2018 15:07:50'!
test03readFromWhenTheInitilValueIsMinusInfinityTheResultIsNegativeInfinity

	self assertThat: '-Infinity' isEqualTo: Float negativeInfinity! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/8/2018 15:08:34'!
test04readFromWhenTheInitilValueIsANegativeNumberTheResultIsThatNumberNegated
	
	self assertThat: '-3' isEqualTo: -3! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/8/2018 15:09:00'!
test05readFromWhenTheInitilValueIsJustANumberTheResultIsThatNumber
	
	self assertThat: '3' isEqualTo: 3! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/9/2018 16:32:38'!
test06readFromWhenTheInitialValueIncludesRadixItIsConvertedWithTheIndicatedRadixBase
	|radixBase number initualNumber |
	
	radixBase := '2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: 30! !

!NumberTest methodsFor: 'test' stamp: 'jmv 9/10/2018 10:28:22'!
test07readFromWhenTheInitialValueIncludesAnInvalidRadixItRaisesAnError
	|radixBase number initualNumber |
	
	radixBase := '0r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	
	self should: [ initualNumber asNumber ] raise: Error description: 'Invalid radix'! !

!NumberTest methodsFor: 'test' stamp: 'GC 9/9/2018 16:32:50'!
test08readFromWhenTheInitialValueIncludesANegativeRadixItConvertsTheNumberWithTheIndicatedRadixBase
	|radixBase number initualNumber |
	
	radixBase := '-2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: -30! !

!NumberTest methodsFor: 'test' stamp: 'jmv 9/10/2018 10:28:25'!
test09readFromWhenTheInitialValueIsEmptyItRaisesAnError
	
	self should: [ Number readFrom: '' ] raise: Error description: 'At least one digit expected here'! !

!NumberTest methodsFor: 'support' stamp: 'GC 9/8/2018 14:45:07'!
assert: expected is: actual
	^ self
		assert: (expected == actual)
		description: (self comparingStringBetween: expected and: actual)! !

!NumberTest methodsFor: 'support' stamp: 'GC 9/8/2018 15:06:11'!
assertThat: aNumber isEqualTo: anExpectedNumber

	| result |
	result := self subject: aNumber.
	
	self assert: result equals: anExpectedNumber! !

!NumberTest methodsFor: 'support' stamp: 'GC 9/8/2018 14:48:11'!
subject: aString

	| aStream |
	aStream := ReadStream on: aString.

	^ Number readFrom: aStream! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'sd 6/5/2005 08:59'!
testBasicNew

	self should: [SmallInteger basicNew] raise: TestResult error. ! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 1/4/2017 08:16:27'!
testMaxVal

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger maxVal = 16r3FFFFFFF) ]
		ifFalse: [
			self assert: (SmallInteger maxVal = 16rFFFFFFFFFFFFFFF) ]! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'jmv 1/4/2017 08:17:27'!
testMinVal

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger minVal = -16r40000000) ]
		ifFalse: [
			self assert: (SmallInteger minVal = -16r1000000000000000) ]! !

!SmallIntegerTest methodsFor: 'testing - Class Methods' stamp: 'sd 6/5/2005 08:59'!
testNew

	self should: [SmallInteger new] raise: TestResult error. ! !

!SmallIntegerTest methodsFor: 'testing - arithmetic' stamp: 'sd 6/5/2005 08:59'!
testDivide

	self assert: 2 / 1 = 2.
	self assert: (3 / 2) isFraction.
	self assert: 4 / 2 = 2.
	self should: [ 1 / 0 ] raise: ZeroDivide.! !

!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 5/7/2012 21:35'!
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

!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 5/7/2012 21:35'!
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

!SmallIntegerTest methodsFor: 'testing - printing' stamp: 'jmv 1/9/2014 23:41'!
testPrintPaddedWith

	self assert: (123 printStringBase: 2 length: 10 padded: true) = '0001111011'.
	self assert: (123 printStringBase: 8 length: 10 padded: true) = '0000000173'.
	self assert: (123 printStringBase: 10 length: 10 padded: true) = '0000000123'.
	self assert: (123 printStringBase: 16 length: 10 padded: true) = '000000007B'! !

!SmallIntegerTest methodsFor: 'testing - printing' stamp: 'jmv 1/4/2017 08:22:13'!
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

!SmallIntegerTest methodsFor: 'tests - zero behavior' stamp: 'jmv 7/2/2019 11:50:44'!
testZeroRaisedToNegativePower
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"

	self should: [0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.

	self should: [-0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1.0] raise: ZeroDivide.! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 18:33'!
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

!RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 18:16'!
nSamplesByDefault

	^1000! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 17:59'!
rng

	^self rngClass new! !

!RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 18:01'!
rngClass

	self subclassResponsibility! !

!RandomTest methodsFor: 'tests' stamp: 'ul 2/27/2015 13:53'!
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

!RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 02:32:34'!
testNextBits
	"Test randomBits: returns integers in the correct range."
	| random |
	random _ self rng.
	0 to: 10 do: [:n| 100 timesRepeat: [self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]].
	0 to: 100 do: [:n| self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]! !

!RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 02:27:56'!
testNextBitsInvalid
	"Requesting a negative amount of random bits should raise an error."
	| random |
	random _ self rng.
	self should: [random nextBits: -1] raise: Error.
	self should: [random nextBits: -100] raise: Error! !

!RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 02:38:58'!
testNextInteger
	"Test nextInteger: returns integers in the correct range."
	| random n |
	random _ self rng.
	1 to: 5 do: [:i| 100 timesRepeat: [self assert: ((random nextInteger: i) between: 1 and: i)]].
	n _ 1<<1000.
	self assert: ((random nextInteger: n) between: 1 and: n)! !

!RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 02:36:52'!
testNextIntegerInvalid
	| random |
	random _ self rng.
	self should: [random nextInteger: -1] raise: Error.
	self should: [random nextInteger: -100] raise: Error.
	self should: [random nextInteger: 0] raise: Error! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 18:38'!
testRandomLargeInteger1

	| random nSamples highBound odds |
	random _ self rng.
	nSamples _ self nSamplesByDefault.
	highBound _ 1 << 64.
	odds _ 0.
	nSamples timesRepeat: [odds _ highBound atRandom: random :: bitAnd: 1 :: + odds].
	self assert: (odds * 2 - nSamples * 5) abs < nSamples  "10% max deviation"! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 18:47'!
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

!RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 03:13:26'!
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

!RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 18:17'!
testSetAtRandom

	| random set nSamples sum |
	random _ self rng.
	set _ Set with: 0 with: 1.
	nSamples _ self nSamplesByDefault.
	sum _ 0.
	nSamples timesRepeat: [sum _ set atRandom: random :: + sum].
	self assert: (sum / nSamples - 0.5) abs < 0.1! !

!RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 20:51'!
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

!RandomTest class methodsFor: 'testing' stamp: 'sqr 3/5/2016 17:54'!
isAbstract

	^self subclasses notEmpty! !

!LaggedFibonacciRandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 18:50'!
nSamplesByDefault

	^self rng majorLag * 100! !

!LaggedFibonacciRandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 18:00'!
rngClass

	^LaggedFibonacciRandom! !

!ParkMiller88RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 19:58'!
rngClass

	^ParkMiller88Random! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 20:06'!
testParkMillerCorrectness
	"The correctness test suggested by the authors"

	| random next |
	random _ self rng seed: 1.
	10000 timesRepeat: [next _ random next].
	self assert: next * 16r7FFFFFFF = 1043618065.0! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'sqr 3/5/2016 18:48'!
testParkMillerInitialValues

	| nSamples random samples |
	nSamples _ self nSamplesByDefault.
	random _ self rng.
	samples _ 1 to: nSamples :: collect: [:each | random next].
	self assert: (samples average - 0.5) abs < 0.1.
	self assert: samples size * 10 > nSamples! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 03:25:04'!
testParkMillerSeedCornerCases
	"Test the insernal state of the Park-Miller generator is correctly set, including corner cases."
	| random m |
	m _ 2147483647.
	{1. 2. (m-1) atRandom. m-1. m-2}
		do: [:each|
			random _ self rng seed: each.
			self assert: (random instVarNamed: #seed) = each]! !

!ParkMiller88RandomTest methodsFor: 'tests' stamp: 'len 6/7/2019 03:25:14'!
testParkMillerSeedRange
	"Test the insernal state of the Park-Miller generator is correctly set, even for arbitrary user-provided seeds."
	| random m |
	m _ 2147483647.
	{1. 1000. 10000000000000000000000. -100. 0. -1. m. m-1. m-2. m+1}
		do: [:each|
			random _ self rng seed: each.
			self assert: ((random instVarNamed: #seed) between: 1 and: m - 1)]! !

!ParkMiller93RandomTest methodsFor: 'setup' stamp: 'sqr 3/5/2016 19:58'!
rngClass

	^ParkMiller93Random! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:29'!
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

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 14:53:59'!
testDayOfWeek

	self assert: '13 May 2017 ' asDate dayOfWeek = 6.
	self assert: '13 May 2017 ' asDate dayOfWeekName = #Saturday! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 19:22:53'!
testInvalidOperations

	self should: [ DateAndTime tomorrow - Week current ] raise: Error.
	self should: [ DateAndTime tomorrow - Date today ] raise: Error! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jpb 8/2/2019 23:33:37'!
testIsDateAndTime
	self assert: (DateAndTime now is: #DateAndTime).! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 19:19:21'!
testLessDateAndTime

	self assert: DateAndTime today - DateAndTime yesterday = 1 days.
	self assert: DateAndTime today - DateAndTime tomorrow = -1 days.
	self assert: DateAndTime tomorrow - DateAndTime yesterday = 2 days.! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:22'!
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

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 4/27/2016 15:04'!
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

!DateTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 15:03:35'!
testCreation

	| date |
	date _ '2014/6/30' asDate.
	self assert: date yearNumber = 2014.
	self assert: date monthIndex = 6.
	self assert: date dayOfMonth = 30.

	date _ '70/12/30' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date _ '12/30/70' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date _ '30/12/70' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	"Quite ambiguous, actually"
	date _ '4/5/6' asDate.
	self assert: date yearNumber = 2006.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 5.

	date _ '15 April 1982' asDate.
	self assert: date yearNumber = 1982.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 15.! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 5/13/2017 19:23:12'!
testInvalidOperations

	self should: [ Date today - DateAndTime tomorrow ] raise: Error.
	self should: [ Date today - Week current ] raise: Error.! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 5/13/2017 19:16:42'!
testLessDate

	self assert: Date today - Date yesterday = 1 days.
	self assert: Date today - Date tomorrow = -1 days.
	self assert: Date tomorrow start - Date yesterday start = 2 days.! !

!DateTest methodsFor: 'test operations' stamp: 'jmv 5/13/2017 14:25:42'!
testLessDuration

	self assert: '13 May 2017' asDate - 3 days = '10 May 2017' asDate! !

!DurationTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 14:56:00'!
testPrintString

	self assert: (Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' ! !

!MonthTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 14:33:22'!
testCreation

	self assert: 'July 1998' asMonth yearNumber = 1998.
	self assert: 'July 1998' asMonth monthIndex = 7.
	self assert: 'July 1998' asMonth monthName = #July.

	self assert: '1998/7' asMonth yearNumber = 1998.
	self assert: '1998/7' asMonth monthIndex = 7.
	self assert: '1998/7' asMonth monthName = #July.! !

!MonthTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 14:55:20'!
testPrintString

	self assert: 'July 1998' asMonth printString = 'July 1998'.
	self assert: '1998/7' asMonth printString = 'July 1998'.
! !

!TimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:32'!
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

!TimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:34'!
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

!TimeTest methodsFor: 'tests' stamp: 'jmv 11/11/2014 09:26'!
testTimePartsArePositive
	self assert: (Time afterMidnight: -10 minutes) minute > 0.
	self assert: (Time afterMidnight: -10 minutes) hour = 23.
	self assert: (Time afterMidnight: -10 minutes) minute = 50.! !

!TimeTest methodsFor: 'Tests' stamp: 'jmv 5/27/2014 22:46'!
testSqueakInquiries
	| timewords |

	timewords := #(0.5 30 62 130 4000 10000 60000 86401) 
		collect: [ :ss | Time humanWordsForSecondsAgo: ss ].
	self assert: 
		timewords = #('a second ago' '30 seconds ago' 'a minute ago' '2 minutes ago' 
			'an hour ago' '2 hours ago' '16 hours ago' 'yesterday').! !

!TimespanTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:42:40'!
testIsTimespan
	self assert: (Timespan current is: #Timespan).! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 15:29:29'!
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

!WeekTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 15:28:58'!
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

!WeekTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 19:22:16'!
testInvalidOperations

	self should: [ Week current - Date tomorrow ] raise: Error.
	self should: [ Week current - DateAndTime tomorrow ] raise: Error.! !

!WeekTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 14:55:27'!
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

!WeekTest methodsFor: 'test class methods' stamp: 'jmv 5/13/2017 14:58:57'!
testWeekDayNames

	self assert: (Week indexOfDay: #Sunday) = 7.
	self assert: (Week nameOfDay: 7) = #Sunday! !

!YearTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 15:19:06'!
testCreation

	self assert: (Year yearNumber: 1998) yearNumber = 1998.
	self assert: '2008' asYear yearNumber = 2008.
	self assert: '2008' asYear start = '2008/01/01' asDate start! !

!YearTest methodsFor: 'tests' stamp: 'jmv 5/13/2017 15:17:42'!
testIncluding

	self assert: (Year including: '12 May 2017 ' asDate start) yearNumber = 2017.
	self assert: (Year including: (Week including: '12 May 2017 ' asDate start)) yearNumber = 2017! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:03:52'!
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

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:04:31'!
test02removePropertiesRestoresPenultimalLiteralToSelector

	| thisMethod propertyName |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:04:52'!
test03removePropertiesKeepsSelectorIfMethodDoesNotHaveProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	"I have to remove it twice because when running with live typing methods always have method properties - Hernan"
	thisMethod removeProperties.
	thisMethod removeProperties.

	self assert: thisMethod penultimateLiteral equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:05:22'!
test04withPropertiesDoIfSelectorEvaluatesSecondBlockWhenMethodHasNoProperties

	| thisMethod methodSelector |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod 
		withPropertiesDo: [ :properties | self fail ]
		ifSelector: [ :selector | methodSelector := selector ].
		
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: methodSelector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:05:40'!
test05hasLiteralSuchThatTravelsListeralsWhenMethodHasProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:05:54'!
test06hasLiteralSuchThatTravelsListeralsWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteralSuchThat: [ :aLiteral | aLiteral = #class ]).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:06:51'!
test0701withPropertiesDoReturnsNilIfMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.

	self assert: (thisMethod withPropertiesDo: [ :properties | self fail ]) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:07:21'!
test07withPropertiesDoEvaluatesBlockOnlyWhenMethodHasProperties

	| thisMethod propertyName methodProperties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.

	thisMethod withPropertiesDo: [ :properties | methodProperties := properties ].
	
	"I don't assert inside the block because if no block is evaluated the test passes - Hernan"	
	self assert: (methodProperties at: propertyName)! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:07:47'!
test08hasLiteralThoroughReturnTrueWhenMethodHasPropertiesAndHasLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self assert: (thisMethod hasLiteralThorough: #class).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:08:08'!
test09hasLiteralThoroughReturnFalseWhenMethodHasPropertiesAndHasNoLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: true.
	
	self deny: (thisMethod hasLiteralThorough: 'abc' asSymbol).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:08:21'!
test10hasLiteralThoroughReturnTrueWhenMethodHasNoPropertiesAndHasLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod hasLiteralThorough: #class).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:08:34'!
test11hasLiteralThoroughReturnFalseWhenMethodHasNoPropertiesAndHasNoLiteral

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self deny: (thisMethod hasLiteralThorough: 'abc' asSymbol).
	
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:08:51'!
test12pragmaAtReturnsPragmaIfExist

	<test>
	
	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) key equals: #test! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:09:07'!
test13pragmaAtReturnsNilIfPragmaDoesNotExist

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	
	self assert: (thisMethod pragmaAt: #test) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:09:26'!
test14pragmasReturnsCollectionOfMethodPragmas

	<test>
	
	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas size equals: 1.
	self assert: pragmas first key equals: #test! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:09:49'!
test15pragmasReturnsEmptyCollectionWhenMethodHasNoProperties

	| thisMethod pragmas |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	pragmas := thisMethod pragmas.
	
	self assert: pragmas isEmpty ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:14:18'!
test16propertiesReturnsAnNewAdditionalMethodStateWhenNoPropertiesHasBeenSet

	| thisMethod properties |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	properties := thisMethod properties.
	
	self assert: properties isEmpty.
	self assert: properties selector equals: thisContext selector.
	self deny: properties == thisMethod properties ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:14:35'!
test17propertiesReturnsTheAlreadySetAdditionalMethodStateWhenHasProperties

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := thisMethod properties.
	self assert: (properties includesKey: propertyName).
	self assert: properties selector equals: thisContext selector.
	! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:17:28'!
test18propertyKeysAndValuesDoIteratesOverProperties

	| propertyName thisMethod properties |
	
	propertyName := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: true.
	
	properties := OrderedCollection new.
	thisMethod propertyKeysAndValuesDo: [ :key :value | properties add: key -> value ].
	
	self assert: properties size equals: 1.
	self assert: properties first equals: propertyName -> true! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 07:20:11'!
test19propertyKeysAndValuesDoDoesNotEvaluateBlockWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	thisMethod propertyKeysAndValuesDo: [ :key :value | self fail ]! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 16:58:44'!
test20propertyValueAtReturnsPropertyValueWhenSetted

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName) equals: propertyValue ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 16:56:20'!
test21propertyValueAtReturnsNilWhenPropertyIsNotFound

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: (thisMethod propertyValueAt: #abc) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 16:55:00'!
test22propertyValueAtReturnsNilWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty) isNil! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 16:58:18'!
test23propertyValueAtIfAbsetReturnsPropertyValueWhenFound

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: propertyName ifAbsent: [ self fail ]) equals: propertyValue ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/3/2019 16:59:33'!
test24propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenNotFound

	| propertyName propertyValue thisMethod |
	
	propertyName := #testProperty.
	propertyValue := 1.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: propertyName put: propertyValue.
	
	self assert: (thisMethod propertyValueAt: #abc ifAbsent: [ propertyValue + 1]) equals: propertyValue + 1 ! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:39:17'!
test25propertyValueAtIfAbsetEvaluatesIfAbsentBlockWhenMethodHasNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: (thisMethod propertyValueAt: #testProperty ifAbsent: [ 1 ]) equals: 1! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:44:05'!
test26propertyValueAtPutCreatesPropertiesWithNewOne

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	propertyValue := thisMethod propertyValueAt: testProperty put: 1.
	
	self assert: propertyValue equals: 1.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 1! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:44:40'!
test27propertyValueAtPutReplacesPropertyIfAlreadyExisted

	| thisMethod testProperty propertyValue |
	
	testProperty := #testProperty.
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: testProperty put: 1.
	propertyValue := thisMethod propertyValueAt: testProperty put: 2.
	
	self assert: propertyValue equals: 2.
	self assert: (thisMethod propertyValueAt: testProperty) equals: 2! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:45:06'!
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

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:47:40'!
test29selectorReturnsMethodSelectorWhenNoProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod removeProperties.
	
	self assert: thisMethod selector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:48:40'!
test30selectorReturnsMethodSelectorWhenHasProperties

	| thisMethod |
	
	thisMethod := self class>>thisContext selector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	self assert: thisMethod selector equals: thisContext selector! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:54:03'!
test31SettingSelectorWhenHasNoProperties

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod removeProperties.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]! !

!CompiledMethodTest methodsFor: 'tests - properties' stamp: 'HAW 1/7/2019 15:54:47'!
test32SettingSelectorWhenHasProperties

	| currentSelector thisMethod |
	
	currentSelector := thisContext selector.
	thisMethod := self class>>currentSelector.
	thisMethod propertyValueAt: #testProperty put: 1.
	
	[ thisMethod selector: #test.
	self assert: thisMethod selector equals: #test ] ensure: [ thisMethod selector: currentSelector ]! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:41:19'!
indexOfIv1
	
	^self class indexOfInstanceVariable: 'iv1'! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:44:50'!
indexOfIv2
	
	^self class indexOfInstanceVariable: 'iv2'! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:40:38'!
iv1

	^iv1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:46:19'!
iv1: anObject

	iv1 := anObject ! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 16:00:45'!
iv1ReadWrite

	iv1 := iv1 yourself! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:41:59'!
iv1_notGetter

	^iv1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:46:58'!
iv1_notSetter: anObject

	iv1 := anObject ! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:45:13'!
iv2

	^iv2 yourself! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:48:08'!
iv2: anObject

	iv2 := 1! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:40:33'!
test01isGetterWhenSameSelectorAsInstanceVariableAndOnlyReturnsThatVariable

	self assert: (self class>>#iv1 isGetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:42:29'!
test02isNotGetterWhenDifferentSelectorAsInstanceVariable

	self deny: (self class>>#iv1_notGetter isGetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:44:13'!
test03isNotGetterWhenSameSelectorAsInstanceVariableButDoesOtherThingBesidesReturningVariable

	self deny: (self class>>#iv2 isGetterOf: 'iv2' at: self indexOfIv2)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:46:04'!
test04isSetterWhenSameKeywordSelectorAsInstanceVariableAndOnlySetsTheVariable

	self assert: (self class>>#iv1: isSetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:46:52'!
test05isNotSetterWhenDifferentKeywordSelectorAsInstanceVariable

	self deny: (self class>>#iv1_notSetter: isSetterOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:47:57'!
test06isNotSetterWhenSameKeywordSelectorAsInstanceVariableButDoesNotAssignParameter

	self deny: (self class>>#iv2: isSetterOf: 'iv2' at: self indexOfIv2)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:58:54'!
test07AccessorDescriptionIsGetterForAGetter

	self assert: 'getter' equals: (self class>>#iv1 accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:59:05'!
test08AccessorDescriptionIsSetterForASetter

	self assert: 'setter' equals: (self class>>#iv1: accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 15:59:34'!
test09AccessorDescriptionIsReadForMethodThatReadsInstanceVariable

	self assert: 'read' equals: (self class>>#iv1_notGetter accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 16:00:19'!
test10AccessorDescriptionIsWriteForMethodThatWritesInstanceVariable

	self assert: 'write' equals: (self class>>#iv1_notSetter: accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!CompiledMethodTest methodsFor: 'tests - accessors' stamp: 'HAW 4/9/2020 16:01:06'!
test11AccessorDescriptionIsWriteReadForMethodThatWritesAndReadInstanceVariable

	self assert: 'write/read' equals: (self class>>#iv1ReadWrite accessorDescriptionOf: 'iv1' at: self indexOfIv1)! !

!DelayTest methodsFor: 'testing-limits' stamp: 'laza 9/5/2009 08:49'!
testBounds
	"self run: #testBounds"
	
	self should: [Delay forMilliseconds: -1] raise: Error.
	self shouldnt: [Delay forMilliseconds: SmallInteger maxVal + 1] raise: Error.
	self shouldnt: [(Delay forMilliseconds: Float pi) wait] raise: Error. "Wait 3ms"
! !

!DelayTest methodsFor: 'testing-limits' stamp: 'jmv 10/8/2012 22:17'!
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

!DelayTest methodsFor: 'testing-limits' stamp: 'ar 9/21/2009 22:12'!
testMultiSchedule
	"Ensure that scheduling the same delay twice raises an error"
	| delay |
	delay := Delay forSeconds: 1.
	delay schedule.
	self should:[delay schedule] raise: Error.
! !

!MonitorTest methodsFor: 'examples' stamp: 'md 3/19/2006 21:15'!
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

!MonitorTest methodsFor: 'examples' stamp: 'md 3/19/2006 21:19'!
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

!MonitorTest methodsFor: 'examples' stamp: 'ul 9/10/2011 16:55'!
testMonitorNotGainingUnwantedSignalsDuringUnwinding

	| monitor process |
	monitor := Monitor new.
	process := [ monitor critical: [ monitor wait ] ] newProcess.
	process resume.
	50 milliSeconds asDelay wait.
	process terminate.
	self assert: 1 equals: ((monitor instVarNamed: #mutex) instVarNamed: #excessSignals)! !

!ProcessTest methodsFor: 'tests' stamp: 'jmv 11/4/2011 22:17'!
testAtomicSuspend
	"Test atomic suspend of foreign processes"

	| list p sema |
	sema := Semaphore new.
	p := [sema wait] newProcess.
	p resume.
	Processor yield.
	self shouldnt: [list := p suspendPrimitivelyOrFail] raise: Error.
	self assert: list == sema.
! !

!SemaphoreTest methodsFor: 'private' stamp: 'tlk 5/5/2006 13:26'!
criticalError
	Processor activeProcess terminate! !

!SemaphoreTest methodsFor: 'testing' stamp: 'tlk 5/5/2006 13:27'!
testCritical
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled! !

!SemaphoreTest methodsFor: 'testing' stamp: 'tlk 5/5/2006 13:28'!
testCriticalIfError
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError ifError:[]]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 6/15/2009 21:24'!
testSemaAfterCriticalWait	"self run: #testSemaAfterCriticalWait"
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [ s critical: []] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: [ (s instVarNamed: #excessSignals) = 1 ]! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 6/15/2009 21:17'!
testSemaInCriticalWait	"self run: #testSemaInCriticalWait"
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical:[]] newProcess.
	p resume.
	Processor yield.
	self assert:[p suspendingList == s].
	p terminate.
	self assert:[(s instVarNamed: #excessSignals) = 0]! !

!SemaphoreTest methodsFor: 'testing' stamp: 'jmv 11/4/2011 22:12'!
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

!SemaphoreTest methodsFor: 'testing' stamp: 'ar 3/23/2009 17:01'!
testWaitTimeoutMSecs
	"Ensure that waitTimeoutMSecs behaves properly"

	"Ensure that a timed out waitTimeoutMSecs: returns true from the wait"
	self assert: (Semaphore new waitTimeoutMSecs: 50) == true.

	"Ensure that a signaled waitTimeoutMSecs: returns false from the wait"
	self assert: (Semaphore new signal waitTimeoutMSecs: 50) == false.
! !

!BecomeTest methodsFor: 'testing' stamp: 'jmv 1/29/2019 12:03:40'!
testBecome
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUpWBecome ] raise: MethodInCallStackToBecomeInvalid! !

!BecomeTest methodsFor: 'testing' stamp: 'jmv 1/29/2019 12:03:42'!
testShapeMutation
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUp ] raise: MethodInCallStackToBecomeInvalid! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 7/8/2018 20:03:35'!
testAndAndAndAndTrueTable

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth] and: [fifth]) 
				equals: ((((first and: [second]) and: [third]) and: [ fourth ]) and: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 7/8/2018 20:03:46'!
testAndAndAndTrueTable

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first and: [second] and: [third] and: [fourth]) 
				equals: (((first and: [second]) and: [third]) and: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	! !

!BooleanTest methodsFor: 'and tests' stamp: 'HAW 7/8/2018 20:04:00'!
testAndAndTrueTable

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first and: [second] and: [third]) 
				equals: ((first and: [second]) and: [third]) ]
		forCombinationOf: 3
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 7/8/2018 20:04:10'!
testOrOrOrOrTrueTable

	self 
		evaluate: [ :first :second :third :fourth :fifth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth] or: [fifth]) 
				equals: ((((first or: [second]) or: [third]) or: [ fourth ]) or: [ fifth ]) ]
		forCombinationOf: 5
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 7/8/2018 20:04:18'!
testOrOrOrTrueTable

	self 
		evaluate: [ :first :second :third :fourth | 
			self 
				assert: (first or: [second] or: [third] or: [fourth]) 
				equals: (((first or: [second]) or: [third]) or: [ fourth ]) ]
		forCombinationOf: 4
		
	
	
	! !

!BooleanTest methodsFor: 'or tests' stamp: 'HAW 7/8/2018 20:04:25'!
testOrOrTrueTable

	self 
		evaluate: [ :first :second :third | 
			self 
				assert: (first or: [second] or: [third]) 
				equals: ((first or: [second]) or: [third]) ]
		forCombinationOf: 3
		
	
	
	! !

!BooleanTest methodsFor: 'combination generation' stamp: 'HAW 7/8/2018 19:51:52'!
evaluate: aBlock collectingValuesInto: values at: aPosition 
	
	aPosition = 0 
		ifTrue: [ aBlock valueWithArguments: values ]
		ifFalse: [ 
			#(true false) do: [ :value | 
				values at: aPosition put: value. 
				self evaluate: aBlock collectingValuesInto: values at: aPosition - 1 ]]! !

!BooleanTest methodsFor: 'combination generation' stamp: 'HAW 7/8/2018 19:50:53'!
evaluate: aBlock forCombinationOf: aNumberOfValues

	self evaluate: aBlock collectingValuesInto: (Array new: aNumberOfValues) at: aNumberOfValues 
	! !

!BooleanTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:13:00'!
testIsBoolean
	self assert: (true is: #Boolean).
	self assert: (false is: #Boolean).! !

!CharacterSetTest methodsFor: 'testing includes' stamp: 'HAW 6/12/2019 17:32:08'!
testIncludesReturnsFalseForCharacterNotIncluded

	self deny: ((CharacterSet with: $a) includes: $b)! !

!CharacterSetTest methodsFor: 'testing includes' stamp: 'HAW 6/12/2019 17:31:45'!
testIncludesReturnsFalseForObjectsThatAreNotCharacters

	self deny: ((CharacterSet with: $a) includes: 1)! !

!CharacterSetTest methodsFor: 'testing includes' stamp: 'HAW 6/12/2019 17:32:24'!
testIncludesReturnsTrueForCharacterIncluded

	self assert: ((CharacterSet with: $a) includes: $a)! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 10/6/2010 22:08'!
testCaseConversion
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' collect: [ :c | c asLowercase ]) = 'año comé tomá camión'.
	self assert:('año Comé tomá Camión' collect: [ :c | c asUppercase ]) = 'AÑO COMÉ TOMÁ CAMIÓN'! !

!CharacterTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:10:55'!
testIsCharacter
	self assert: ($a is: #Character).
	self assert: ($X is: #Character).
	self assert: (Character cr is: #Character).! !

!CharacterTest methodsFor: 'testing' stamp: 'jmv 10/6/2010 22:05'!
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

!CharacterTest methodsFor: 'testing' stamp: 'jmv 10/6/2010 22:10'!
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

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:09'!
testFromUtf8
	"
	CharacterTest new testFromUtf8
	"
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: '24')) hex = '16r24'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'C2A2')) hex = '16rA2'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'E282AC')) hex = '16r20AC'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'F0A4ADA2')) hex = '16r24B62'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 9/5/2016 20:48:25'!
testSomeLatinCharsFromUtf8
	"
	CharacterTest new testSomeLatinCharsFromUtf8
	"
	| bytes string |
	bytes _ (ByteArray readHexFrom: 'C3A1C3A5C3A6C3B1C386C2A5C3BC') readStream.
	string _ String streamContents: [ :strm |
		[bytes atEnd ] whileFalse: [
			strm nextPut: (Character codePoint: (Character nextUnicodeCodePointFromUtf8: bytes )) ]].
	self assert: string = 'áåæñÆ¥ü'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 9/4/2016 13:11:55'!
testSomeLatinCharsToUtf8
	"
	CharacterTest new testSomeLatinCharsToUtf8
	"
	| characters bytes |
	characters _ 'áåæñÆ¥ü' readStream.
	bytes _ ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: characters next codePoint ]].
	self assert: bytes hex = 'C3A1C3A5C3A6C3B1C386C2A5C3BC'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:09'!
testToUtf8
	"
	CharacterTest new testToUtf8
	"
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r0024) hex =  '24'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r00A2) hex =  'C2A2'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r20AC) hex = 'E282AC'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r024B62) hex = 'F0A4ADA2'! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNilIfNotNil0Arg

	self assert: (5@4 ifNil: [#foo] ifNotNil: [#bar]) = #bar.
	self assert: (nil ifNil: [#foo] ifNotNil: [#bar]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNilIfNotNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNilIfNotNil1Arg

	self assert: (5@4 ifNil: [#foo] ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNil: [#foo] ifNotNil: [:a | a printString]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNilIfNotNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = '5@4'.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNil0Arg

	self assert: (5@4 ifNotNil: [#foo]) = #foo.
	self assert: (nil ifNotNil: [#foo]) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNil0ArgAsVar

	| block |
	block := [#foo].
	self assert: (5@4 ifNotNil: block) = #foo.
	self assert: (nil ifNotNil: block) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNil1Arg

	self assert: (5@4 ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString]) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNil1ArgAsVar

	| block |
	block := [:a | a printString].
	self assert: (5@4 ifNotNil: block) = '5@4'.
	self assert: (nil ifNotNil: block) = nil! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNilIfNil0Arg

	self assert: (5@4 ifNotNil: [#foo] ifNil: [#bar]) = #foo.
	self assert: (nil ifNotNil: [#foo] ifNil: [#bar]) = #bar! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNilIfNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:46'!
testIfNotNilIfNil1Arg

	self assert: (5@4 ifNotNil: [:a | a printString] ifNil: [#foo]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString] ifNil: [#foo]) = #foo! !

!IfNotNilTests methodsFor: 'tests' stamp: 'jmv 11/16/2010 08:47'!
testIfNotNilIfNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = '5@4'.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessor
	"
	ProcessorTest new testGrabProcessor
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
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
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessor
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
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
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessorOnlyForNoTimeout
	"
	ProcessorTest new testGrabProcessorOnlyForNoTimeout
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
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
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 1000
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: (updates = tries)
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: (observations = tries)
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) <60
		description: 'The low priority process was not preempted right after #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessorOnlyForTimeout
	"
	1 to: 40 do: [ :i | i print. ProcessorTest new testGrabProcessorOnlyForTimeout ]
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait consistentCount inconsistentCount |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
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
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25 							"About half the time should preempt it before it finishes"
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 15) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ (Delay forMilliseconds: 2) wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 3
		description: 'It seems the low priority process finished always before the timeout'.
	self
		assert: inconsistentCount > 5
		description: 'It seems the low priority process was not preempted at the timeout'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testValueUnpreemptively
	"
	ProcessorTest new testValueUnpreemptively
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
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
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] valueUnpreemptively
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
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
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'private' stamp: 'jmv 9/22/2009 09:42'!
wasteTime: iterations
	iterations timesRepeat: [1.23 asString asNumber]! !

!SpecialSelectorsTest methodsFor: 'testing' stamp: 'jmv 6/5/2017 12:17:20'!
testCollectionAtSymbol
	self assert: (#(1 2 3) @ #(10 20 30)) class == Array! !

!SpecialSelectorsTest methodsFor: 'testing' stamp: 'jmv 6/5/2017 12:16:49'!
testNumberAtSymbol
	self assert: (1@2) class == Point! !

!StrikeFontTest methodsFor: 'testing' stamp: 'jmv 6/11/2020 16:30:55'!
testBaseAndDerivedFont
	"
	StrikeFontTest new testBaseAndDerivedFont
	"

	| base italic italicItalic boldItalic italicBold boldItalic2 underlined underlinedItalic italicUnderlined struckThrough1 struckThrough2 |
	base _ FontFamily defaultFamilyPointSize: 14.
	self assert: base isBaseFont.
	italic _ base italic.
	self deny: italic isBaseFont.
	italicItalic _ italic italic.
	self assert: italicItalic == italic.
	boldItalic _ base bold italic.
	self deny: boldItalic isBaseFont.
	boldItalic2 _ base boldItalic.
	italicBold _ italic bold.
	self assert: boldItalic == italicBold.
	self assert: boldItalic == boldItalic2.
	underlined _ base underlined.
	self deny: underlined isBaseFont.
	underlinedItalic _ underlined italic.
	italicUnderlined _ italic underlined.
	self assert: underlinedItalic == italicUnderlined.
	struckThrough1 _ base italic underlined bold struckThrough.
	struckThrough2 _ base bold underlined italic struckThrough.
	self assert: struckThrough1 == struckThrough2! !

!StrikeFontTest methodsFor: 'testing' stamp: 'jmv 6/11/2020 16:25:48'!
testBaseAndDerivedFontSanity
	"
	StrikeFontTest new testBaseAndDerivedFontSanity
	"

	StrikeFont allInstances do: [ :f |
		f isBaseFont 
			ifTrue: [
				self assert: (f instVarNamed: 'baseFont') isNil.
				self assert: f baseFont == f.
				self assert: (f instVarNamed: 'derivativeFonts') notNil ]
			ifFalse: [
				self assert: f baseFont notNil.
				self assert: (f instVarNamed: 'derivativeFonts') isNil.
				self assert: (f baseFont emphasized: f emphasis) == f ] ]! !

!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:17'!
testAsUtf8
	"
	 StringTest new testAsUtf8
	"
	self assert: 'A¢¤' asUtf8 hex = '41C2A2E282AC'! !

!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 6/10/2019 09:23:03'!
testAsUtf8WithNCRs
	"
	 StringTest new testAsUtf8WithNCRs
	"
	| stringWithDecimalNCRs stringWithHexNCRs utf8 |
	utf8 _ ByteArray readHexFrom: ('CE BC 20 CE A8 20 CF 89 20 54 68 65 20 64 65 63 6F 6D 70 6F 73 69 74 69 6F 6E 20 6D 61 70 70 69 6E 67 20 69 73 20 3C EC B8 A0 2C 20 E1 86 B8 3E 2C 20 61 6E 64 20 6E 6F 74 20 3C E1 84 8E 2C 20 E1 85 B3 2C 20 31 31 42 38 3E 2E 0A 3C 70 3E 54 68 65 20 74 69 74 6C 65 20 73 61 79 73 20 E2 80 AB D7 A4 D7 A2 D7 99 D7 9C D7 95 D7 AA 20 D7 94 D7 91 D7 99 D7 A0 D7 90 D7 95 D7 9D 2C 20 57 33 43 E2 80 AC 20 69 6E 20 48 65 62 72 65 77 3C 2F 70 3E 0A 61 62 63 E0 A4 95 E0 A4 96 E0 A5 80 E5 9C 8B E9 9A 9B F0 90 8E 84 F0 90 8E 94 F0 90 8E 98' reject: [ :char  | char isSeparator ]).

	stringWithDecimalNCRs _ String fromUtf8: utf8 hex: false trimLastNull: false.
	stringWithHexNCRs _ String fromUtf8: utf8 hex: true trimLastNull: false.

	self assert: stringWithDecimalNCRs = '&#956; &#936; &#969; The decomposition mapping is <&#52768;, &#4536;>, and not <&#4366;, &#4467;, 11B8>.
<p>The title says &#8235;&#1508;&#1506;&#1497;&#1500;&#1493;&#1514; &#1492;&#1489;&#1497;&#1504;&#1488;&#1493;&#1501;, W3C&#8236; in Hebrew</p>
abc&#2325;&#2326;&#2368;&#22283;&#38555;&#66436;&#66452;&#66456;'.

	self assert: (stringWithDecimalNCRs asUtf8: true) = utf8.

	self assert: stringWithHexNCRs = '&#x03BC; &#x03A8; &#x03C9; The decomposition mapping is <&#xCE20;, &#x11B8;>, and not <&#x110E;, &#x1173;, 11B8>.
<p>The title says &#x202B;&#x05E4;&#x05E2;&#x05D9;&#x05DC;&#x05D5;&#x05EA; &#x05D4;&#x05D1;&#x05D9;&#x05E0;&#x05D0;&#x05D5;&#x05DD;, W3C&#x202C; in Hebrew</p>
abc&#x0915;&#x0916;&#x0940;&#x570B;&#x969B;&#x10384;&#x10394;&#x10398;'.

	self assert: (stringWithHexNCRs asUtf8: true) = utf8! !

!StringTest methodsFor: 'tests - converting' stamp: 'jmv 9/19/2016 09:55:39'!
testBase64

	self 
		assert: 'SGVsbG8gV29ybGQ=' base64Decoded = 'Hello World' asByteArray;
		assert: 'Hello World' asByteArray base64Encoded = 'SGVsbG8gV29ybGQ=';
		assert: (String new: 100 withAll: $x) asByteArray base64Encoded  =
'eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4
eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eA==' ! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/3/2018 10:42:26'!
testAfterBlanksEndsWith

	self assert: (' abc' afterBlanksEndsWith: 'abc').
	self assert: ('abc' afterBlanksEndsWith: 'abc').
	
	self deny: (' ab' afterBlanksEndsWith: 'abc').
	self deny: (' ab' afterBlanksEndsWith: 'a').
	self deny: ('' afterBlanksEndsWith: 'abc').
	self deny: (' a a bc' afterBlanksEndsWith: 'a bc').! !

!StringTest methodsFor: 'testing' stamp: 'jmv 10/6/2010 22:08'!
testCaseConversion
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' asLowercase) = 'año comé tomá camión'.
	self assert: ('año Comé tomá Camión' asUppercase) = 'AÑO COMÉ TOMÁ CAMIÓN'! !

!StringTest methodsFor: 'testing' stamp: 'jmv 9/1/2009 14:12'!
testEncompassParagraph1

	self assert: ('a' encompassParagraph: (1 to: 0)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (1 to: 1)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (2 to: 1)) = (1 to: 1).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 9/1/2009 14:13'!
testEncompassParagraph2

	self assert: ('ab' encompassParagraph: (1 to: 0)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (3 to: 2)) = (1 to: 2).! !

!StringTest methodsFor: 'testing' stamp: 'jmv 9/1/2009 14:14'!
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

!StringTest methodsFor: 'testing' stamp: 'jmv 9/1/2009 14:17'!
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

!StringTest methodsFor: 'testing' stamp: 'jmv 9/1/2009 14:22'!
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

!StringTest methodsFor: 'testing' stamp: 'HAW 12/29/2020 13:29:16'!
testFindSelector 

	self assert: #printOn: equals: '"self printOn:' findSelector.
	self assert: #printOn: equals: 'self printOn:"' findSelector.
	self assert: #printOn: equals: '"self printOn:"' findSelector.
	self assert: #printOn: equals: 'self printOn:' findSelector.
! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/3/2018 11:03:25'!
testFirstNoBlankIndex

	self assert: 1 equals: 'abc' firstNoBlankIndex.
	self assert: 2 equals: ' abc' firstNoBlankIndex.
	self assert: 0 equals: '' firstNoBlankIndex ! !

!StringTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:11:49'!
testIsString
	self assert: ('Hello World' is: #String).
	self assert: ('' is: #String).! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/3/2018 11:10:05'!
testLastNoBlankIndex

	self assert: 3 equals: 'abc' lastNoBlankIndex.
	self assert: 3 equals: 'abc ' lastNoBlankIndex.
	self assert: 4 equals: ' abc ' lastNoBlankIndex.
	self assert: 0 equals: '' lastNoBlankIndex ! !

!StringTest methodsFor: 'testing' stamp: 'jmv 12/17/2012 10:51'!
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

!StringTest methodsFor: 'testing' stamp: 'jmv 10/6/2010 22:03'!
testSorting
"
self new testSorting
"

	self assert: 'a' < 'á'.
	self assert: ('a' < 'Á') not.
	self assert: 'A' < 'á'.
	self assert: 'A' < 'Á'.
	self assert: 'á' < 'b'.
	self assert: ('á' < 'B') not.
	self assert: 'Á' < 'b'.
	self assert: 'Á' < 'B'.

	self assert: ('a' caseSensitiveLessOrEqual: 'á').
	self deny: ('a' caseSensitiveLessOrEqual: 'Á').
	self assert: ('A' caseSensitiveLessOrEqual: 'á').
	self assert: ('A' caseSensitiveLessOrEqual: 'Á').
	self assert: ('á' caseSensitiveLessOrEqual: 'b').
	self deny: ('á' caseSensitiveLessOrEqual: 'B').
	self assert: ('Á' caseSensitiveLessOrEqual: 'b').
	self assert: ('Á' caseSensitiveLessOrEqual: 'B').

	self assert: ('a' caseInsensitiveLessOrEqual: 'á').
	self assert: ('a' caseInsensitiveLessOrEqual: 'Á').
	self assert: ('A' caseInsensitiveLessOrEqual: 'á').
	self assert: ('A' caseInsensitiveLessOrEqual: 'Á').
	self assert: ('á' caseInsensitiveLessOrEqual: 'b').
	self assert: ('á' caseInsensitiveLessOrEqual: 'B').
	self assert: ('Á' caseInsensitiveLessOrEqual: 'b').
	self assert: ('Á' caseInsensitiveLessOrEqual: 'B').! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/3/2018 11:00:28'!
testWithBlanksTrimmed

	self assert: 'abc' equals: ' abc' withBlanksTrimmed.
	self assert: 'abc' equals: 'abc ' withBlanksTrimmed.
	self assert: 'abc' equals: ' abc ' withBlanksTrimmed.
	self assert: 'a b c' equals: ' a b c ' withBlanksTrimmed.
	self assert: '' equals: '' withBlanksTrimmed.
	! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/2/2018 20:35:28'!
testWithoutLeadingBlanks

	self assert: 'abc' equals: ' abc' withoutLeadingBlanks.
	self assert: 'abc' equals: 'abc' withoutLeadingBlanks.
	self assert: '' withoutLeadingBlanks isEmpty
! !

!StringTest methodsFor: 'testing' stamp: 'HAW 6/8/2019 18:18:47'!
testWithoutSeparators

	self assert: 'abc' equals: 'abc' withoutSeparators.
	self assert: 'abc' equals: ' a b c ' withoutSeparators! !

!StringTest methodsFor: 'testing' stamp: 'HAW 8/2/2018 20:35:32'!
testWithoutTrailingBlanks

	self assert: 'abc' equals: 'abc ' withoutTrailingBlanks.
	self assert: 'abc' equals: 'abc' withoutTrailingBlanks.
	self assert: '' withoutTrailingBlanks isEmpty
! !

!StringTest methodsFor: 'tests - grammar' stamp: 'LC 7/5/2020 16:47:10'!
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

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 12/20/2018 14:41:00'!
testCanNotInsertShiftingRightWhenInsertionPointIsBiggerThanSize

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := arrayToInsertTo size + 1.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 12/20/2018 14:39:47'!
testCanNotInsertShiftingRightWhenInsertionPointIsLessThanOne

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := 0.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 12/20/2018 14:27:08'!
testInsertShiftingRightLoosesLastElementAndKeepsPreviousOnes

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 2) equals: #(1 2 3 4)! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 12/20/2018 14:27:21'!
testInsertShiftingRightReplacesElementWhenSizeIsOne

	self assert: (#(1) copy insert: 2 shiftingRightAt: 1) equals: #(2)! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 12/20/2018 14:28:20'!
testInsertShiftingRightReplacesLastElementWhenInsertingAtLastIndex

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 4) equals: #(1 3 4 2)! !

!ArrayTest methodsFor: 'test - testing' stamp: 'jpb 8/2/2019 22:59:46'!
testIsArray
	self assert: (#(1 2 3) is: #Array).
	self assert: (#() is: #Array).! !

!ArrayTest methodsFor: 'test - testing' stamp: 'jpb 8/2/2019 23:09:00'!
testIsCollection
	self assert: (#(1 2 3) is: #Collection).
	self assert: (#() is: #Collection).! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:46:01'!
testAntepenultimateIfAbsentReturnsAntepenultimateIfExists

	self assert: 1 equals: (#(1 2 3) antepenultimateIfAbsent: [ self fail ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:47:18'!
testAntepenultimateIfAbsentValuesIfAbsentBlockWhenNoAntepenultimate

	self assert: 0 equals: (#(1 2) antepenultimateIfAbsent: [ 0 ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:48:13'!
testAntepenultimateReturnsAntepenultimateIfExists

	self assert: 1 equals: #(1 2 3) antepenultimate! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:49:10'!
testAntepenultimateSignalsErrorWhenCollectionIsNotBigEnough

	self
		should: [ #(1 2) antepenultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/4/2020 00:37:18'!
testLastIfEmptyReturnsLastWhenExist

	self assert: 1 equals: (#(1) lastIfEmpty: [ self fail ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/4/2020 00:38:24'!
testLastIfEmptyValuesEmptyBlockWhenEmpty

	self assert: 1 equals: (#() lastIfEmpty: [ 1 ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/4/2020 00:43:47'!
testLastSignalsErrorWhenEmpty

	self
		should: [ #() last ]
		raise: Error
		withMessageText: Collection emptyCollectionDescription! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:37:15'!
testPenultimateIfAbsentReturnsPenultimateIfExists

	self assert: 2 equals: (#(2 3) penultimateIfAbsent: [ self fail ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:41:03'!
testPenultimateIfAbsentValuesIfAbsentBlockWhenNoPenultimate

	self assert: 2 equals: (#(1) penultimateIfAbsent: [ 2 ])! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:41:31'!
testPenultimateReturnsPenultimateIfExists

	self assert: 2 equals: #(2 3) penultimate! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 5/3/2020 00:43:44'!
testPenultimateSignalsErrorWhenCollectionIsNotBigEnough

	self
		should: [ #(1) penultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 9/3/2020 18:39:55'!
testIsType
	self assert: (#[1.2 2.4 3.6 ] is: #Collection).
	self assert: (#[1.2 2.4 3.6] asFloat32Array is: #Float32Array).! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 9/3/2020 18:39:53'!
testZeroArrayDividend

	self assert: #[1.2 2.4 0.0 ] asFloat32Array / #[1.2 1.2 1.2 ] asFloat32Array = #[1.0 2.0 0.0 ] asFloat32Array ! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 9/3/2020 18:40:16'!
testZeroArrayDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] asFloat32Array / #[1.0 1.0 0.0 ] asFloat32Array] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: #[1.0 1.0 0.0 0.0 ] asFloat32Array ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ] asFloat32Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat32Array
			divideBy: #[1.0 1.0 0.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] asFloat32Array
			divideBy: #[0.0 1.0 1.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592] asFloat32Array.
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 9/3/2020 18:39:33'!
testZeroDividend

	self assert: #[1.2 2.4 3.6 0.0 ] asFloat32Array / 1.2 = #[1.0 2.0 3.0 0.0 ] asFloat32Array ! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 9/3/2020 18:39:30'!
testZeroDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] asFloat32Array / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ] asFloat32Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat32Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] asFloat32Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592] asFloat32Array.
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:40:09'!
divideByArrayIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: #[0.0 2.0 3.141592] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:40:13'!
divideByArrayOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: #[1.0 1.0 1.0] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:40:03'!
divideByArrayZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat32Array
		divideBy: #[0.0 2.0 3.141592] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:40:00'!
divideByScalarIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:39:00'!
divideByScalarOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 9/3/2020 18:39:02'!
divideByScalarZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat32Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jpb 8/2/2019 23:07:59'!
testIsCollection
	self assert: (#[1.0 2.0 3.141592 0.0] is: #Collection).
	self assert: (#[-100.0 -100.0 -100.0 -200.0 ] is: #Collection).! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 7/9/2018 09:38:50'!
testZeroArrayDividend

	self assert: #[1.2 2.4 0.0 ] / #[1.2 1.2 1.2 ] = #[1.0 2.0 0.0 ] ! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 7/9/2018 09:51:49'!
testZeroArrayDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] / #[1.0 1.0 0.0 ]] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: #[1.0 1.0 0.0 0.0 ] ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ].

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: #[1.0 1.0 0.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: #[0.0 1.0 1.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592].
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 7/9/2018 09:39:14'!
testZeroDividend

	self assert: #[1.2 2.4 3.6 0.0 ] / 1.2 = #[1.0 2.0 3.0 0.0 ] ! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 7/9/2018 09:53:42'!
testZeroDivisor

	"Test implicit exception"
	| |
	self should: [ #[1.2 2.4 3.6 ] / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ].

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592].
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:52:51'!
divideByArrayIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:52:56'!
divideByArrayOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[1.0 1.0 1.0]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:53:05'!
divideByArrayZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:38:35'!
divideByScalarIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592]
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:38:39'!
divideByScalarOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592]
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 7/9/2018 09:38:43'!
divideByScalarZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592]
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'as yet unclassified' stamp: 'jmv 7/30/2020 21:58:18'!
testSum
	| a |
	self assert: ({0.0. 1.0. Float pi. Float e} asFloat64Array + 1) = ({0.0+1. 1.0+1. Float pi+1. Float e+1} asFloat64Array).
	a _ {0.0. 1.0. Float pi. Float e} asFloat64Array.
	self assert: (a + a) = (a * 2 )! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 3/29/2017 13:50:30'!
testBlockClosuresCanBeUsedAsHandlingCondition

	self 
		shouldnt: [ [ Error signal ] on: [:anException | true ] do: [ :anError | ] ]
		raise: Error.
		
	! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 3/29/2017 13:48:37'!
testFilterConditionWorksAsExcpetedWhenHandlingException

	self
		shouldnt: [ [ Error signal ] on: Error - ZeroDivide do: [ :anError |  ] ]
		raise: Error.
		
	self
		should: [ [ ZeroDivide signal ] on: Error - ZeroDivide do: [ :anError | self fail ]]
		raise: Error.

! !

!ExceptionHandlingConditionTest methodsFor: 'exception handling tests' stamp: 'HAW 3/29/2017 13:49:45'!
testOrConditionWorksAsExcpetedWhenHandlingException

	self 
		shouldnt: [ [ Error signal ] on: Error, Notification do: [ :anError | ] ]
		raise: Error.
		
	self 
		shouldnt: [ [ Notification signal ] on: Error, Notification do: [ :anError |  ] ]
		raise: Notification 

	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:06'!
testCanCreateFilterConditionWithExceptionTypeAndExceptionType

	| condition |
	
	condition := Error - ZeroDivide .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:15'!
testCanCreateFilterConditionWithExceptionTypeAndFilterCondition

	| condition |
	
	condition := Error - (ZeroDivide - Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Halt new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:10'!
testCanCreateFilterConditionWithExceptionTypeAndOrCondition

	| condition |
	
	condition := Error - (ZeroDivide, Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self deny: (condition handles: Halt new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:35'!
testCanCreateFilterConditionWithFilterConditionAndExceptionType

	| condition |
	
	condition := (Error - Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:43'!
testCanCreateFilterConditionWithFilterConditionAndFilterCondition

	| condition |
	
	condition := (Error - Halt) - (ZeroDivide - Notification).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Notification new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:39'!
testCanCreateFilterConditionWithFilterConditionAndOrCondition

	| condition |
	
	condition := (Error - Halt) - (Notification, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: Notification new).
	self deny: (condition handles: ZeroDivide new).
! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:20'!
testCanCreateFilterConditionWithOrConditionAndExceptionType

	| condition |
	
	condition := (Error, Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:29'!
testCanCreateFilterConditionWithOrConditionAndFilterCondition

	| condition |
	
	condition := (Error, Halt) - (ZeroDivide - ArithmeticError).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'filter condition tests' stamp: 'HAW 3/29/2017 15:24:25'!
testCanCreateFilterConditionWithOrConditionAndOrCondition

	| condition |
	
	condition := (Error, Halt) - (ArithmeticError, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 15:21:29'!
testCanCreateOrConditionWithExceptionTypeAndExceptionType

	| handlingCondition |
	
	handlingCondition := Error, Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 17:59:39'!
testCanCreateOrConditionWithExceptionTypeAndFilterCondition
	
	| handlingCondition |
	
	handlingCondition := Error, (ArithmeticError - ZeroDivide).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: ArithmeticError new).
	self deny: (handlingCondition handles: ZeroDivide new).
	
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 15:21:37'!
testCanCreateOrConditionWithExceptionTypeAndOrCondition

	| handlingCondition |
	
	handlingCondition := Error, (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 17:56:49'!
testCanCreateOrConditionWithFilterConditionAndExceptionType

	| condition |
	
	condition := (Error - ZeroDivide), ArithmeticError .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 17:56:57'!
testCanCreateOrConditionWithFilterConditionAndFilterCondition

	| condition |
	
	condition := (Error - ZeroDivide), (ArithmeticError - DomainError).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: DomainError new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 17:57:02'!
testCanCreateOrConditionWithFilterConditionAndOrCondition

	| condition |
	
	condition := (ArithmeticError - ZeroDivide), (Error, Halt).
	
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 15:22:01'!
testCanCreateOrConditionWithOrConditionAndExceptionType
	
	| handlingCondition |
	
	handlingCondition := (Error, Halt), Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	
	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 17:57:09'!
testCanCreateOrConditionWithOrConditionAndFilterCondition

	| exceptionSet |
	
	exceptionSet := (Error, Notification), (ArithmeticError - ZeroDivide).
	
	self assert: (exceptionSet handles: Error new).
	self assert: (exceptionSet handles: Notification new).
	self assert: (exceptionSet handles: ArithmeticError new).
	self deny: (exceptionSet handles: ZeroDivide new).

	! !

!ExceptionHandlingConditionTest methodsFor: 'or condition tests' stamp: 'HAW 3/29/2017 15:22:06'!
testCanCreateOrConditionWithOrConditionAndOrCondition

	| handlingCondition |
	
	handlingCondition := (Error, Halt), (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	! !

!ExceptionHandlingConditionTest methodsFor: 'environment preconditions' stamp: 'sqr 6/26/2019 11:50:31'!
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

!ExceptionTests methodsFor: 'private' stamp: 'md 3/25/2003 23:40'!
assertSuccess: anExceptionTester
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 6/1/2004 21:54'!
testDoubleOuterPass
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 6/1/2004 21:54'!
testDoublePassOuter
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:43'!
testDoubleResume
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'!
testNonResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 10/10/2015 16:22'!
testResignalAs

	| answer |
	answer := [
		[3 zork]
			on: ZeroDivide
			do: [:ex | ex return: 5]
	] on: Error do: [:ex | ex resignalAs: ZeroDivide].
	self assert: answer == 5! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 11/15/2015 11:42'!
testResignalAsUnwinds

	| unwound answer |
	unwound := false.
	answer := [
		[3 zork]
			on: ZeroDivide do: [:ex | self assert: unwound.  ex return: 5]
	] on: Error do: [:ex | [ex resignalAs: ZeroDivide] ifCurtailed: [unwound := true]].
	self assert: answer == 5! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'!
testResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'!
testSignalFromHandlerActionTest
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'!
testSimpleEnsure
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:45'!
testSimpleEnsureTestWithError
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:46'!
testSimpleEnsureTestWithNotification
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:45'!
testSimpleEnsureTestWithUparrow
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:46'!
testSimpleIsNested
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:41'!
testSimpleOuter
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:42'!
testSimplePass
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:43'!
testSimpleResignalAs
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'!
testSimpleResume
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'!
testSimpleRetry
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:47'!
testSimpleRetryUsing
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest ) ! !

!ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'!
testSimpleReturn
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest ) ! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'SqR 8/28/2014 22:45'!
testHandlerFromAction
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [
		[
			[self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
		] on: Error do: [ :ex | 3 / 0 ]
	] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer'! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 6/1/2004 21:59'!
testNonResumableOuter

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 6/1/2004 22:00'!
testNonResumablePass

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 11/15/2010 11:39'!
testResumableOuter

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result = 5
! !

!ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 11/15/2010 11:39'!
testResumablePass

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
	self assert: result = 4
! !

!ExceptionTests methodsFor: 'testing' stamp: 'brp 10/21/2004 16:42'!
testNoTimeout
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest ) ! !

!ExceptionTests methodsFor: 'testing' stamp: 'brp 10/21/2004 16:41'!
testTimeoutWithZeroDuration
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest ) ! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'!
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

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 11/4/2011 22:00'!
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
	self deny: unwindStarted.
	Processor yield.
	self assert: unwindStarted.
	self deny: unwindFinished.
	p terminate.
	self assert: unwindFinished.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'!
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

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:14'!
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

!ArrayLiteralTest methodsFor: 'tests' stamp: 'jpb 8/2/2019 23:05:08'!
testIsArray
	self assert: ({1. 2. 'hello'. #Number} is: #Array).! !

!ArrayLiteralTest methodsFor: 'tests' stamp: 'HAW 10/26/2019 18:24:11'!
testReservedIdentifiers
	
	self assert: #(nil true false) equals: {nil. true. false}.! !

!ArrayLiteralTest methodsFor: 'tests' stamp: 'HAW 10/26/2019 18:24:52'!
testSymbols

	self assert: #(#nil #true #false #'nil' #'true' #'false') equals: {#nil. #true. #false. #nil. #true. #false}.! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:13'!
test01isAtSuperclassWhenIndexIsInSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtSuperclass: 1).
	self assert: (analyzer isAtSuperclass: self class superclass name size)! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:19'!
test02isNotAtSuperclassWhenIndexIsOutsideSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtSuperclass: 0).
	self deny: (analyzer isAtSuperclass: self class superclass name size + 1).
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:25'!
test03isAtClassNameWhenIndexIsInClassName

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) - 1).
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size - 1).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:29'!
test04isNotAtClassNameWhenIndexIsOutsideClassName

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) - 2).
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:35'!
test05isAtInstanceVariablesWhenIndexIsInInstanceVariablesString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1')).
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1') + 2).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:40'!
test06isNotAtInstanceVariablesWhenIndexIsOutsideInstanceVariablesString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''')).
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''') + 4).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:45'!
test07CanAccessSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: self class superclass equals: analyzer superclass! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:50'!
test08isAtCategoryWhenIndexIsInCategoryString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString)).
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size - 1).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:07:55'!
test09isNotAtCategoryWhenIndexIsOutsideCategoryString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) - 1).
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 10/11/2019 16:51:32'!
test10InstanceVariablesIsTheSecondParameter

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer instanceVariableNamesPositionForClassDefinition ) 
			equals: 'instanceVariableNames:' ]! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 21:06:49'!
test11CategoryIsTheFifthParameter

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer categoryPosition) 
			equals: 'category:' ]! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 4/17/2019 18:16:08'!
thisClassDefinitionNode
	
	 ^self class methodNodeFor: self class definition noPattern: true ! !

!ClosureCompilerTest methodsFor: 'source' stamp: 'jmv 3/13/2012 11:29'!
closureCases
	^#(
'| n |
n := 1.
^n + n'

'| i |
i := 0.
[i := i + 1.
 i <= 10] whileTrue.
^i'

'[:c :s| | mn |
mn := Compiler new
		compile: (c sourceCodeAt: s)
		in: c
		notifying: nil
		ifFail: [self halt].
mn generate: #(0 0 0 0).
{mn blockExtentsToTempsMap.
  mn encoder schematicTempNames}]
			value: ArrayLiteralTest
			value: #testSymbols'

'inject: thisValue into: binaryBlock
	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue'

'runBinaryConditionalJumps: assertPrintBar
	"CogIA32CompilerTests new runBinaryConditionalJumps: false"
	| mask reg1 reg2 reg3 |
	mask := 1 << self processor bitsInWord - 1.
	self concreteCompilerClass dataRegistersWithAccessorsDo:
		[:n :get :set|
		n = 0 ifTrue: [reg1 := get].
		n = 1 ifTrue: [reg2 := set].
		n = 2 ifTrue: [reg3 := set]].
	#(	(JumpAbove > unsigned)			(JumpBelowOrEqual <= unsigned)
		(JumpBelow < unsigned)			(JumpAboveOrEqual >= unsigned)
		(JumpGreater > signed)			(JumpLessOrEqual <= signed)
		(JumpLess < signed)				(JumpGreaterOrEqual >= signed)
		(JumpZero = signed)				(JumpNonZero ~= signed)) do:
		[:triple|
		[:opName :relation :signednessOrResult| | opcode jumpNotTaken jumpTaken nop memory bogus |
		self resetGen.
		opcode := CogRTLOpcodes classPool at: opName.
		self gen: CmpRR operand: 2 operand: 1.
		jumpTaken := self gen: opcode.
		self gen: MoveCqR operand: 0 operand: 0.
		jumpNotTaken := self gen: Jump.
		jumpTaken jmpTarget: (self gen: MoveCqR operand: 1 operand: 0).
		jumpNotTaken jmpTarget: (nop := self gen: Nop).
		memory := self generateInstructions.
		bogus := false.
		self pairs: (-2 to: 2)  do:
			[:a :b| | taken |
			self processor
				reset;
				perform: reg2 with: a signedIntToLong;
				perform: reg3 with: b signedIntToLong.
			[self processor singleStepIn: memory.
			 self processor pc ~= nop address] whileTrue.
			taken := (self processor perform: reg1) = 1.
			assertPrintBar
				ifTrue:
					[self assert: taken = (signednessOrResult == #unsigned
											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]
											ifFalse: [a perform: relation with: b])]
				ifFalse:
					[Transcript
						nextPutAll: reg2; nextPut: $(; print: a; nextPutAll: '') ''; nextPutAll: relation; space;
						nextPutAll: reg3; nextPut: $(; print: b; nextPutAll: '') = '';
						print: taken; cr; flush.
					 taken = (signednessOrResult == #unsigned
											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]
											ifFalse: [a perform: relation with: b]) ifFalse:
						[bogus := true]]].
			 bogus ifTrue:
				[self processor printRegistersOn: Transcript.
				 Transcript show: (self processor disassembleInstructionAt: jumpTaken address In: memory); cr]]
					valueWithArguments: triple]'

'mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys asSortedCollection
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector| | subMap tempVector numTemps |
		subMap := Dictionary new.
		"Find how many temp slots there are (direct & indirect temp vectors)
		 and for each indirect temp vector find how big it is."
		tempNameTupleVector do:
			[:tuple|
			tuple last isArray
				ifTrue:
					[subMap at: tuple last first put: tuple last last.
					 numTemps := tuple last first]
				ifFalse:
					[numTemps := tuple last]].
		"create the temp vector for this scope level."
		tempVector := Array new: numTemps.
		"fill it in with any indirect temp vectors"
		subMap keysAndValuesDo:
			[:index :size|
			tempVector at: index put: (Array new: size)].
		"fill it in with temp nodes."
		tempNameTupleVector do:
			[:tuple| | itv |
			tuple last isArray
				ifTrue:
					[itv := tempVector at: tuple last first.
					 itv at: tuple last last
						put: (aDecompilerConstructor
								codeTemp: tuple last last - 1
								named: tuple first)]
				ifFalse:
					[tempVector
						at: tuple last
						put: (aDecompilerConstructor
								codeTemp: tuple last - 1
								named: tuple first)]].
		"replace any indirect temp vectors with proper RemoteTempVectorNodes"
		subMap keysAndValuesDo:
			[:index :size|
			tempVector
				at: index
				put: (aDecompilerConstructor
						codeRemoteTemp: index
						remoteTemps: (tempVector at: index))].
		"and update the entry in the map"
		map at: startpc put: tempVector].
	^map'

 'gnuifyFrom: inFileStream to: outFileStream

"convert interp.c to use GNU features"

	| inData beforeInterpret inInterpret inInterpretVars beforePrimitiveResponse inPrimitiveResponse |

	inData := inFileStream upToEnd withSqueakLineEndings.
	inFileStream close.

	"print a header"
	outFileStream
		nextPutAll: ''/* This file has been post-processed for GNU C */'';
		cr; cr; cr.

	beforeInterpret := true.    "whether we are before the beginning of interpret()"
	inInterpret := false.     "whether we are in the middle of interpret"
	inInterpretVars := false.    "whether we are in the variables of interpret"
	beforePrimitiveResponse := true.  "whether we are before the beginning of primitiveResponse()"
	inPrimitiveResponse := false.   "whether we are inside of primitiveResponse"
	''Gnuifying''
		displayProgressAt: Sensor mousePoint
		from: 1 to: (inData occurrencesOf: Character crCharacter)
		during:
			[:bar | | lineNumber |
			lineNumber := 0.
			inData linesDo:
				[ :inLine | | outLine extraOutLine caseLabel |
				bar value: (lineNumber := lineNumber + 1).
				outLine := inLine. 	"print out one line for each input line; by default, print out the line that was input, but some rules modify it"
				extraOutLine := nil.   "occasionally print a second output line..."
				beforeInterpret ifTrue: [
					inLine = ''#include "sq.h"'' ifTrue: [
						outLine := ''#include "sqGnu.h"'' ].
					inLine = ''interpret(void) {'' ifTrue: [
						"reached the beginning of interpret"
						beforeInterpret := false.
						inInterpret := true.
						inInterpretVars := true ] ]
				ifFalse: [
				inInterpretVars ifTrue: [
					(inLine findString: ''register struct foo * foo = &fum;'') > 0 ifTrue: [
						outLine := ''register struct foo * foo FOO_REG = &fum;'' ].
					(inLine findString: '' localIP;'') > 0 ifTrue: [
						outLine := ''    char* localIP IP_REG;'' ].
					(inLine findString: '' localFP;'') > 0 ifTrue: [
						outLine := ''    char* localFP FP_REG;'' ].
					(inLine findString: '' localSP;'') > 0 ifTrue: [
						outLine := ''    char* localSP SP_REG;'' ].
					(inLine findString: '' currentBytecode;'') > 0 ifTrue: [
						outLine := ''    sqInt currentBytecode CB_REG;'' ].
					inLine isEmpty ifTrue: [
						"reached end of variables"
						inInterpretVars := false.
						outLine := ''    JUMP_TABLE;''.
						extraOutLine := inLine ] ]
				ifFalse: [
				inInterpret ifTrue: [
					"working inside interpret(); translate the switch statement"
					(inLine beginsWith: ''		case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''		CASE('', caseLabel, '')'' ].
					inLine = ''			break;'' ifTrue: [
						outLine := ''			BREAK;'' ].
					inLine = ''}'' ifTrue: [
						"all finished with interpret()"
						inInterpret := false ] ]
				ifFalse: [
				beforePrimitiveResponse ifTrue: [
					(inLine beginsWith: ''primitiveResponse('') ifTrue: [
						"into primitiveResponse we go"
						beforePrimitiveResponse := false.
						inPrimitiveResponse := true.
						extraOutLine := ''    PRIM_TABLE;'' ] ]
				ifFalse: [
				inPrimitiveResponse ifTrue: [
					inLine = ''	switch (primitiveIndex) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					inLine = ''	switch (GIV(primitiveIndex)) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					(inLine beginsWith: ''	case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''	CASE('', caseLabel, '')'' ].
					inLine = ''}'' ifTrue: [
						inPrimitiveResponse := false ] ]
				] ] ] ].

				outFileStream nextPutAll: outLine; cr.
				extraOutLine ifNotNil: [
					outFileStream nextPutAll: extraOutLine; cr ]]].

	outFileStream close' )! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 6/24/2008 12:28'!
doTestDebuggerTempAccessWith: one with: two
	"Test debugger access for temps"
	| outerContext local1 remote1 |
	outerContext := thisContext.
	local1 := 3.
	remote1 := 1/2.
	self assert: (Compiler new evaluate: 'one' in: thisContext to: self) == one.
	self assert: (Compiler new evaluate: 'two' in: thisContext to: self) == two.
	self assert: (Compiler new evaluate: 'local1' in: thisContext to: self) == local1.
	self assert: (Compiler new evaluate: 'remote1' in: thisContext to: self) == remote1.
	Compiler new evaluate: 'local1 := -3.0' in: thisContext to: self.
	self assert: local1 = -3.0.
	(1 to: 2) do:
		[:i| | local2 r1 r2 r3 r4 |
		local2 := i * 3.
		remote1 := local2 / 7.
		self assert: thisContext ~~ outerContext.
		self assert: (r1 := Compiler new evaluate: 'one' in: thisContext to: self) == one.
		self assert: (r2 := Compiler new evaluate: 'two' in: thisContext to: self) == two.
		self assert: (r3 := Compiler new evaluate: 'i' in: thisContext to: self) == i.
		self assert: (r4 := Compiler new evaluate: 'local2' in: thisContext to: self) == local2.
		self assert: (r4 := Compiler new evaluate: 'remote1' in: thisContext to: self) == remote1.
		self assert: (r4 := Compiler new evaluate: 'remote1' in: outerContext to: self) == remote1.
		Compiler new evaluate: 'local2 := 15' in: thisContext to: self.
		self assert: local2 = 15.
		Compiler new evaluate: 'local1 := 25' in: thisContext to: self.
		self assert: local1 = 25.
		{ r1. r2. r3. r4 } "placate the compiler"].
	self assert: local1 = 25.
	self assert: remote1 = (6/7)! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 5/27/2015 13:03'!
supportTestSourceRangeAccessForDecompiledInjectInto: method source: source
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	'_ arg1'
								'do: [:arg5 | temp3 _ arg2 value: temp3 value: arg5]'
								'value: temp3 value: arg5'
								'_ arg2 value: temp3 value: arg5'
								'temp3 _ arg2 value: temp3 value: arg5'
								'value: temp3 value: arg5'
								'_ arg2 value: temp3 value: arg5'
								'temp3 _ arg2 value: temp3 value: arg5'
								'^temp3')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 6/4/2008 19:44'!
supportTestSourceRangeAccessForDecompiledNoBytecodeInjectInto: method source: source
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	'at: 1 put: t1'
								'do: [:t4 | t3 at: 1 put: (t2 value: (t3 at: 1) value: t4)]'
								'value: (t3 at: 1) value: t4'
								'at: 1 put: (t2 value: (t3 at: 1) value: t4)'
								']'
								'value: (t3 at: 1) value: t4'
								'at: 1 put: (t2 value: (t3 at: 1) value: t4)'
								']'
								'^t3 at: 1')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 12/30/2009 14:17'!
supportTestSourceRangeAccessForInjectInto: method source: source
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	'_ thisValue'
								'do: [:each | nextValue _ binaryBlock value: nextValue value: each]'
								'value: nextValue value: each'
								'_ binaryBlock value: nextValue value: each'
								'nextValue _ binaryBlock value: nextValue value: each'
								'value: nextValue value: each'
								'_ binaryBlock value: nextValue value: each'
								'nextValue _ binaryBlock value: nextValue value: each'
								'^nextValue')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 7/29/2008 17:16'!
supportTestSourceRangeAccessForInjectInto: method source: source selectionSequence: selections
	"Test debugger source range selection for inject:into:"
	| evaluationCount sourceMap debugTokenSequence debugCount |
	DebuggerMethodMap voidMapCache.
	evaluationCount := 0.
	sourceMap := method debuggerMap abstractSourceMap.
	debugTokenSequence := selections collect: [:string| Scanner new scanTokens: string].
	debugCount := 0.
	thisContext
		runSimulated: [(1 to: 2)
						withArgs:
							{	0.
								[:sum :each|
								 evaluationCount := evaluationCount + 1.
								 sum + each]}
						executeMethod: method]
		contextAtEachStep:
			[:ctxt| | range debugTokens |
			(ctxt method == method
			and: ["Exclude the send of #blockCopy: or #closureCopy:copiedValues: and braceWith:with:
				    to create the block, and the #new: and #at:'s for the indirect temp vector.
				   This for compilation without closure bytecodes. (Note that at:put:'s correspond to stores)"
				(ctxt willSend
					and: [(#(closureCopy:copiedValues: blockCopy: new: at: braceWith:with:) includes: ctxt selectorToSendOrSelf) not])
				"Exclude the store of the argument into the home context (for BlueBook blocks)
				 and the store of an indirection vector into an initial temp"
				or: [(ctxt willStore
					and: [(ctxt isBlock and: [ctxt pc = ctxt startpc]) not
					and: [(ctxt isBlock not
						and: [(method usesClosureBytecodes and: [ctxt abstractPC = 2])]) not]])
				or: [ctxt willReturn]]]) ifTrue:
				[debugTokens := debugTokenSequence at: (debugCount := debugCount + 1) ifAbsent: [#(bogusToken)].
				 self assert: (sourceMap includesKey: ctxt abstractPC).
				 range := sourceMap at: ctxt abstractPC ifAbsent: [(1 to: 0)].
				 self assert: (Scanner new scanTokens: (source copyFrom: range first to: range last)) = debugTokens]].
	self assert: evaluationCount = 2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 11/3/2016 11:45:16'!
testBlockDoitDecompilation
	"Tests that decompile of a doit block with remote vars executes correcly"
	"Tests that decompilation of a Block, when 'method' of block is equivalent to that compiled by a DoIt, preserves the temp names "
	
	| blockSourceStream methodNode block decompiledBlock method |
	blockSourceStream := '|x y| [:a :b | x _ a. y _ b. x + y]' readStream.
	methodNode := Compiler new 
						from: blockSourceStream class: nil class context: nil notifying: nil;
						translate: blockSourceStream noPattern: true ifFail: [nil].
	method _ methodNode generate.
	block := nil withArgs: #() executeMethod: method.
	
	self shouldnt: [decompiledBlock := block decompile] raise: Error.
	self assert: 
 '{[ :arg4 :arg5 |
	temp1 _ arg4.
	temp3 _ arg5.
	temp1 + temp3. ]}' equals: decompiledBlock printString
! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 1/17/2011 00:13'!
testBlockNumbering
	"Test that the compiler and CompiledMethod agree on the block numbering of a substantial doit."
	"self new testBlockNumbering"
	| methodNode method tempRefs |
	methodNode _
		Parser new
			encoderClass: EncoderForV3PlusClosures;
			parse: 'foo
					| numCopiedValuesCounts |
					numCopiedValuesCounts := Dictionary new.
					0 to: 32 do: [:i| numCopiedValuesCounts at: i put: 0].
					Transcript clear.
					Smalltalk allClassesDo:
						[:c|
						{c. c class} do:
							[:b|
							Transcript nextPut: b name first; endEntry.
							b selectorsAndMethodsDo:
								[:s :m| | pn |
								m isQuick not ifTrue:
									[pn := b parserClass new
												encoderClass: EncoderForV3PlusClosures;
												parse: (b sourceCodeAt: s)
												class: b.
									 pn generate: #(0 0 0 0).
									 [pn accept: nil]
										on: MessageNotUnderstood
										do: [:ex| | msg numCopied |
											msg := ex message.
											(msg selector == #visitBlockNode:
											 and: [(msg argument instVarNamed: ''optimized'') not]) ifTrue:
												[numCopied := (msg argument computeCopiedValues: pn) size.
												 numCopiedValuesCounts
													at: numCopied
													put: (numCopiedValuesCounts at: numCopied) + 1].
											msg setSelector: #==.
											ex resume: nil]]]]].
					numCopiedValuesCounts'
			class: Object.
	method _ methodNode generate: #(0 0 0 0).
	tempRefs _ methodNode encoder blockExtentsToTempsMap.
	self assert: tempRefs keys asSet = method startpcsToBlockExtents values asSet! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 1/17/2011 00:13'!
testBlockNumberingForInjectInto
	"Test that the compiler and CompiledMethod agree on the block numbering of Collection>>inject:into:
	 and that temp names for inject:into: are recorded."
	"self new testBlockNumberingForInjectInto"
	| methodNode method tempRefs |
	methodNode := Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: (Collection sourceCodeAt: #inject:into:)
						class: Collection.
	method := methodNode generate: #(0 0 0 0).
	tempRefs := methodNode encoder blockExtentsToTempsMap.
	self assert: tempRefs keys asSet = method startpcsToBlockExtents values asSet.
	self assert: ((tempRefs includesKey: (0 to: 6))
				and: [(tempRefs at: (0 to: 6)) hasEqualElements: #(('thisValue' 1) ('binaryBlock' 2) ('nextValue' (3 1)))]).
	self assert: ((tempRefs includesKey: (2 to: 4))
				and: [(tempRefs at: (2 to: 4)) hasEqualElements: #(('each' 1) ('binaryBlock' 2) ('nextValue' (3 1)))])! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 07:20:04'!
testDebuggerTempAccess

	"This test also fails in Squeak. Check if it is ever fixed."
	self shouldFail: [ self doTestDebuggerTempAccessWith: 1 with: 2 ]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 5/27/2015 13:52'!
testInjectIntoDecompilationsEncoderForV3PlusClosures
	"Test various compilations decompile to the same code for a method sufficiently
	 simple that this is possible and sufficiently complex that the code generated
	 varies between the compilations."
	"self new testInjectIntoDecompilationsEncoderForV3PlusClosures"
	| source |

	source := (Collection sourceCodeAt: #inject:into:) asString.
	{ EncoderForV3PlusClosures } do:
		[:encoderClass| | method |
		method := (Parser new
							encoderClass: encoderClass;
							parse: source
							class: Collection)
						generate: #(0 0 0 0).
		self assert: (Scanner new scanTokens: method decompileString)
					= #(inject: arg1 into: arg2
							| temp3 |
							temp3 _ arg1 .
							self do: [ ':arg5' | temp3 _ arg2 value: temp3 value: arg5 ] .
							^ temp3 . )]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 5/27/2015 13:04'!
testInjectIntoDecompiledDebugs
	"Test various debugs of the decompiled form debug correctly."
	"self new testInjectIntoDecompiledDebugs"
	| source |

	source := (Collection sourceCodeAt: #inject:into:) asString.
	{ EncoderForV3PlusClosures } do:
		[:encoderClass| | method |
		method := (Parser new
							encoderClass: encoderClass;
							parse: source
							class: Collection)
						generate: #(0 0 0 0).
		self supportTestSourceRangeAccessForDecompiledInjectInto: method source: method decompileString]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 7/24/2009 11:51'!
testInlineBlockCollectionEM1
	| a1 b1 i1 a2 b2 i2 we wb |
	b1 := OrderedCollection new.
	i1 := 1.
	[a1 := i1.
	 i1 <= 3] whileTrue:
		[b1 add: [a1].
		i1 := i1 + 1].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	i2 := 1.
	we := [a2 := i2. i2 <= 3].
	wb := [b2 add: [a2]. i2 := i2 + 1].
	we whileTrue: wb. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 3/7/2009 11:25'!
testInlineBlockCollectionLR1
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | col add: [ each ] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 3/7/2009 11:39'!
testInlineBlockCollectionLR2
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | #(1) do: [:ignored| col add: [ each ]] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 3/9/2009 11:00'!
testInlineBlockCollectionLR3
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
	self assert: (col collect: [ :each | each value ]) asArray = (2 to: 12) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 7/22/2009 16:55'!
testInlineBlockCollectionSD1
	| a1 b1 a2 b2 |
	b1 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a1 := i.
		b1 add: [a1]].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a2 := i.
		b2 add: [a2]] yourself. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 6/4/2008 15:20'!
testSourceRangeAccessForClosureBytecodeInjectInto
	"Test debugger source range selection for inject:into: for a version compiled with closures"
	"self new testSourceRangeAccessForClosureBytecodeInjectInto"
	| source method |
	source := (Collection sourceCodeAt: #inject:into:) asString.
	method := (Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: source
						class: Collection)
					generate: (Collection compiledMethodAt: #inject:into:) trailer.
	self supportTestSourceRangeAccessForInjectInto: method source: source! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 6/4/2008 11:40'!
testSourceRangeAccessForInjectInto
	"Test debugger source range selection for inject:into: for the current version of the method"
	"self new testSourceRangeAccessForInjectInto"
	self supportTestSourceRangeAccessForInjectInto: (Collection compiledMethodAt: #inject:into:)
		source: (Collection sourceCodeAt: #inject:into:) asString! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 6/15/2008 11:26'!
testTempNameAccessForInjectInto
	"self new testTempNameAccessForInjectInto"
	| methodNode method evaluationCount block debuggerMap |
	methodNode := Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: (Collection sourceCodeAt: #inject:into:)
						class: Collection.
	method := methodNode generate: #(0 0 0 0).
	debuggerMap := DebuggerMethodMap forMethod: method methodNode: methodNode.
	evaluationCount := 0.
	block := [:prev :each| | theContext tempNames |
			evaluationCount := evaluationCount + 1.
			theContext := thisContext sender.
			tempNames := debuggerMap tempNamesForContext: theContext.
			self assert: (tempNames hasEqualElements: tempNames).
			#('thisValue' 'each' 'binaryBlock' 'nextValue')
				with: { 0. each. block. prev}
				do: [:tempName :value|
					self assert: (debuggerMap namedTempAt: (tempNames indexOf: tempName) in: theContext) == value.
					tempName ~= 'each' ifTrue:
						[self assert: (debuggerMap namedTempAt: (tempNames indexOf: tempName) in: theContext home) == value]]].
	(1 to: 10) withArgs: { 0. block } executeMethod: method.
	self assert: evaluationCount = 10! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/20/2008 09:40'!
methodWithCopiedAndAssignedTemps
	| blk "0w" a "0w" b "0w" c "0w" t "0w" r1 "0w" r2 "0w" |
	a := 1. "1w"
	b := 2. "1w"
	c := 4. "1w"
	t := 0. "1w"
	blk "5w" := ["2" t  "3w" := t "3r" + a "3r" + b "3r" + c "3r" ] "4".
	r1 "5w" := blk "5r" value.
	b "5w" := -100.
	r2 "5w" := blk "5r" value.
	^r1 "5r" -> r2 "5r" -> t "5r"

	"a: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 b: main(read(),write(0,1,5)), block(read(3),write()) => remote; write follows contained read
	 blk: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 c: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 r1: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 r2: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 t: main(read(5),write(0,1)), block(read(3),write(3)) => remote; read follows contained write"


	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/19/2008 20:45'!
methodWithCopiedAndPostClosedOverAssignedTemps
	| blk a b c r1 r2 |
	a := 1.
	b := 2.
	c := 4.
	blk := [a + b + c].
	r1 := blk value.
	b := nil.
	r2 := blk value.
	r1 -> r2

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndPostClosedOverAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/19/2008 20:10'!
methodWithCopiedTemps
	| a b c r |
	a := 1.
	b := 2.
	c := 4.
	r := [a + b + c] value.
	b := nil.
	r

	"Parser new
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class"

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/19/2008 14:24'!
methodWithOptimizedBlocks
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| b | b := 'notNil'. b].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocks)
		class: self class"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/19/2008 14:24'!
methodWithOptimizedBlocksA
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocksA)
		class: self class"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 5/19/2008 14:12'!
methodWithVariousTemps
	| classes total totalLength |
	classes := self withAllSuperclasses.
	total := totalLength := 0.
	classes do: [:class| | className |
		className := class name.
		total := total + 1.
		totalLength := totalLength + className size].
	^total -> totalLength

	"Parser new
		parse: (self class sourceCodeAt: #methodWithVariousTemps)
		class: self class"! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample01
	| blockClosure materialized |
	blockClosure _ self bc01.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample02
	| blockClosure materialized |
	blockClosure _ self bc02.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample03
	| blockClosure materialized |
	blockClosure _ self bc03.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample04
	| blockClosure materialized |
	blockClosure _ self bc04.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample05
	| blockClosure materialized |
	blockClosure _ self bc05.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample06
	| blockClosures materialized firstRun |

	blockClosures _ self bc06.
	firstRun _ blockClosures second value.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self assert: firstRun = materialized second value.
	self deny: blockClosures second value = materialized second value.

	blockClosures _ self bc06.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self deny: firstRun = materialized second value.
	self assert: blockClosures second value = materialized second value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample07
	| blockClosures materialized |

	blockClosures _ self bc07.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self deny: blockClosures third value = materialized third value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc07.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample08
	| blockClosures materialized |

	blockClosures _ self bc08.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self deny: (blockClosures second value: 7) = (materialized second value: 7).
	self deny: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).

	blockClosures _ self bc08.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample09
	| blockClosures materialized |

	blockClosures _ self bc09.
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].

	self assert: (blockClosures first value: $c) = (materialized first value: $c).
	self assert: (blockClosures second value: $d value: #e) = (materialized second value: $d value: #e).
	self assert: (blockClosures third value: $d value: $e) = (materialized third value: $d value: $e).! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample10
	| blockClosure1 blockClosure2 blockClosure3 value materialized1 materialized2a materialized2b materialized3a materialized3b materialized3c |

	blockClosure1 _ self bc10.
	blockClosure2 _ blockClosure1 withFirstArg: 1.
	blockClosure3 _ (blockClosure1 withFirstArg: 1) withFirstArg: 2.
	value _ blockClosure1 value: 1 value: 2.
	self assert: (blockClosure2 value: 2) = value.
	self assert: blockClosure3 value = value.

	materialized1 _ blockClosure1 veryDeepCopy.
	materialized2a _ materialized1 withFirstArg: 1.
	materialized2b _ blockClosure2 veryDeepCopy.
	materialized3a _ (materialized1 withFirstArg: 1) withFirstArg: 2.
	materialized3b _ materialized2b withFirstArg: 2.
	materialized3c _ blockClosure3 veryDeepCopy.
	self assert: (materialized1 value: 1 value: 2) = value.
	self assert: (materialized2a value: 2) = value.
	self assert: (materialized2b value: 2) = value.
	self assert: materialized3a value = value.
	self assert: materialized3b value = value.
	self assert: materialized3c value = value.
! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 9/26/2019 23:19:41'!
testSample11
	| blockClosures materialized |

	blockClosures _ self bc11.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc11.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.! !

!ClosureSerializationTest methodsFor: 'aux' stamp: 'jmv 9/26/2019 23:19:41'!
gimme5
	^1+4! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc01
	^[ self gimme5 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc02
	^[ ivar + 4 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc03
	^[ ivar _ ivar + 3 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc04
	| t |
	t _ self gimme5.
	^[ t * 2 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc05
	| t |
	t _ self gimme5.
	^[ t _ t * 2 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc06
	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{[ u * 2 ]. [ t * 2 ].[t _ t*3]}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc07
	| a b c d e f g |
	a _ self gimme5 + 1.
	b _ self gimme5 + 2.
	c _ self gimme5 + 3.
	d _ self gimme5 + 4.
	e _ self gimme5 + 5.
	f _ self gimme5 + 6.
	g _ self gimme5 + 7.
	^{[ a * 10 + b * 10 + c * 10 + d * 10 + e + f + g ]. [ a _ 7. a * 2 ].[ c _ 9. d _ d*10. a * 10 + b * 10 + c + d]}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc08

	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{
		[ | x y z | x _ t. y _ x*0+2. z _ x * t. {u * 2. x. y. z} ].
		[ :a | t * 2 + a ].
		[|x y | t _ t*3. y _ t*2. {x. t. y}].
		[:a :b | a * b * t ]
	}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc09

	| clo1 clo2 closure |
	closure := (
			([ :a |
				[ :b |
					clo1 := [ :c | {c. b. a.}]. 
					clo2 := [ :d :e | {a. b. e. d.}]
				]
			]) value: $a
		) value: 'b'.
	^{ clo1. clo2. closure }! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc10

	^ [ :a :b | a + b ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 9/26/2019 23:19:41'!
bc11
	| a b c d e f g |
	a _ self gimme5 + 1.
	b _ self gimme5 + 2.
	c _ self gimme5 + 3.
	d _ self gimme5 + 4.
	e _ self gimme5 + 5.
	f _ self gimme5 + 6.
	g _ self gimme5 + 7.
	{ b. d. e. f } print.
	^{[ g + a * 10 + a + a + g + c ]. [ b _ 7. c _ c * 2 ]}.! !

!ClosureSerializationTest methodsFor: 'setUp/tearDown' stamp: 'jmv 9/26/2019 23:19:41'!
setUp
	ivar _ 7.
	^ivar! !

!ClosureTests methodsFor: 'utilities' stamp: 'lr 3/9/2009 16:48'!
assertValues: anArray
	| values |
	values := collection collect: [ :each | each value ].
	self 
		assert: anArray asArray = values asArray
		description: 'Expected: ' , anArray asArray printString , 
			', but got ' , values asArray printString! !

!ClosureTests methodsFor: 'utilities' stamp: 'cwp 11/16/2009 08:12'!
evaluateCopyOf: aBlock
	aBlock copy value! !

!ClosureTests methodsFor: 'utilities' stamp: 'cwp 11/16/2009 08:12'!
methodWithNonLocalReturn
	self evaluateCopyOf: [^ self].
	self signalFailure: 'Should never reach here'! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 3/11/2009 17:35'!
methodArgument: anObject
	^ [ anObject ]
	! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 3/11/2009 17:33'!
testBlockArgument
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 3/11/2009 17:33'!
testBlockTemp
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'cwp 11/16/2009 08:11'!
testCopyNonLocalReturn
	self
		shouldnt: [self methodWithNonLocalReturn]
		raise: Error! !

!ClosureTests methodsFor: 'testing' stamp: 'jmv 7/3/2019 09:38:17'!
testIsTrivialClosure
	"
	ClosureTests new testIsTrivialClosure
	"
	| tempVar |
	tempVar _ 1.
	self assert: [ 3 + 4 ] isCleanClosure.
	self assert: [ :a | a * 2 ] isCleanClosure.
	self assert: [ Smalltalk size ] isCleanClosure.
	self assert: [ ClosureTests selectors size ] isCleanClosure.
	self assert: [ :blockArg | blockArg printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp _ 7 ] isCleanClosure.
	self assert: [ | c | c _ [ :a :b | a+b ]. c value: 3 value: 4 ] isCleanClosure.

	self assert: [ | outerBlockTemp | [ outerBlockTemp printString ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [ outerBlockTemp _ 7 ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp _ 7 ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp _ 7 ]] ] isCleanClosure.

	self deny: [ | outerBlockTemp | [ outerBlockTemp printString ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [ outerBlockTemp _ 7 ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp printString ]] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp _ 7 ]] isCleanClosure ] value.

	self deny: [ tempVar + 1 ] isCleanClosure.
	self deny: [ tempVar _ 1 ] isCleanClosure.
	self deny: [ ivar + 1 ] isCleanClosure.
	self deny: [ ivar _ 1 ] isCleanClosure.
	self deny: [ ^ true ] isCleanClosure.
	self deny: [ self printString ] isCleanClosure.
	self deny: [ ^ self ] isCleanClosure.
	self deny: [ ClassVar + 1 ] isCleanClosure.
	self deny: [ ClassVar _ 1 ] isCleanClosure! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 3/11/2009 17:36'!
testMethodArgument
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	self assert: block value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 3/11/2009 17:36'!
testMethodTemp
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'running' stamp: 'lr 3/9/2009 16:48'!
setUp
	super setUp.
	collection := OrderedCollection new! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoArgument
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoArgumentNotInlined
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoInsideTemp
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoInsideTempNotInlined
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoOutsideTemp
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 3/10/2009 14:51'!
testToDoOutsideTempNotInlined
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:29'!
testWhileModificationAfter
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		collection add: [ index ].
		index := index + 1 ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:29'!
testWhileModificationAfterNotInlined
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:29'!
testWhileModificationBefore
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:29'!
testWhileModificationBeforeNotInlined
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:52'!
testWhileWithTemp
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 3/10/2009 14:53'!
testWhileWithTempNotInlined
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(1 2 3 4 5)! !

!ContextCompilationTest methodsFor: 'tests' stamp: 'eem 6/19/2008 10:11'!
testVariablesAndOffsetsDo

	"ContextCompilationTest new testVariablesAndOffsetsDo"
	| contextClasses |
	contextClasses := ContextPart withAllSuperclasses, ContextPart allSubclasses asArray.
	contextClasses do:
		[:class|
		class variablesAndOffsetsDo:
			[:var :offset|
			self assert: offset < 0.
			self assert: (class instVarNameForIndex: offset negated) == var]].

	InstructionStream withAllSuperclasses, InstructionStream allSubclasses asArray do:
		[:class|
		(contextClasses includes: class) ifFalse:
			[class variablesAndOffsetsDo:
				[:var :offset|
				(InstructionStream instVarNames includes: var) ifFalse:
					[self assert: offset > 0.
					 self assert: (class instVarNameForIndex: offset) == var]]]]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'sd 9/25/2004 15:30'!
blockingClasses


	^ #(CompiledMethod)! !

!DecompilerTests methodsFor: 'utilities' stamp: 'jmv 9/24/2020 16:56:42'!
checkDecompileMethod: oldMethod
	
	| cls selector oldMethodNode methodNode newMethod oldCodeString newCodeString |
	cls _ oldMethod methodClass.
	selector := oldMethod selector.
	oldMethodNode _ Decompiler new
						decompile: selector
						in: cls
						method: oldMethod.
	[oldMethodNode properties includesKey: #warning]
		whileTrue: [oldMethodNode properties removeKey: #warning].
	oldCodeString _ oldMethodNode decompileString.
	methodNode _ [ Compiler new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil]
						on: SyntaxErrorNotification
						do: [ :ex |
							ex errorMessage = 'Cannot store into'
								ifTrue: [ex return: #badStore].
							ex pass ].
	"Ignore cannot store into block arg errors; they're not our issue."
	methodNode ~~ #badStore ifTrue: [
		newMethod _ methodNode generate: #(0 0 0 0).
		newCodeString := (Decompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString.
		"Decompiler might move temp declarations (keeping valid, equivalent code) and therefore affect the temp numbering.
		Make temp numbering cannonical in decompiler code, to avoid bogus failures"
		oldCodeString _ self withFixedTempNumbering: oldCodeString.
		newCodeString _ self withFixedTempNumbering: newCodeString.
		"(DifferenceFinder displayPatchFrom: oldCodeString to: newCodeString tryWords: true)
			editLabel: 'Decompilation Differences for ', cls name,'>>',selector."
		"(DifferenceFinder displayPatchFrom: oldMethod abstractSymbolic to: newMethod abstractSymbolic tryWords: true)
			editLabel: 'Bytecode Differences for ', cls name,'>>',selector."
		self assert: oldCodeString = newCodeString
			description: cls name asString, ' ', selector asString
			resumable: true ]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'jmv 5/27/2015 13:58'!
decompileClassesSelect: aBlock
	| cls |
	(Smalltalk classNames select: aBlock) do: [ :cn |
		cls _ Smalltalk at: cn.
		 cls selectorsAndMethodsDo: [ :selector :meth |
				self checkDecompileMethod: meth ]]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'jmv 9/24/2020 17:21:21'!
withFixedTempNumbering: aString
	| k code newCode tempStart startAt |
	code _ aString.
	k _ 1.
	startAt _ 1.
	[tempStart _ code findString: 'temp' startingAt: startAt. tempStart > 0] whileTrue: [ | end |
		end _ tempStart. [end <= code size and: [(code at: end) tokenish]] whileTrue: [end _ end + 1]. end _ end-1.
		newCode _ code  copyReplaceTokens: (code copyFrom: tempStart to: end) with: 'tenq', k printString.
		code = newCode ifTrue: [startAt _ tempStart + 1].
		code _ newCode.
		k _ k + 1 ].
	^code! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesAAtoAM
	self decompileClassesSelect: [:cn| cn first = $A and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesANtoAZ
	self decompileClassesSelect: [:cn| cn first = $A and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:57:20'!
testDecompilerInClassesBAtoBM

	self decompileClassesSelect: [:cn| cn first = $B and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:57:44'!
testDecompilerInClassesBNtoBZ

	self decompileClassesSelect: [:cn| cn first = $B and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:57:51'!
testDecompilerInClassesCAtoCM

	self decompileClassesSelect: [:cn| cn first = $C and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:57:58'!
testDecompilerInClassesCNtoCZ

	self decompileClassesSelect: [:cn| cn first = $C and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:06'!
testDecompilerInClassesDAtoDM

	self decompileClassesSelect: [:cn| cn first = $D and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:13'!
testDecompilerInClassesDNtoDZ

	self decompileClassesSelect: [:cn| cn first = $D and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesEAtoEM
	self decompileClassesSelect: [:cn| cn first = $E and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesENtoEZ
	self decompileClassesSelect: [:cn| cn first = $E and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:22'!
testDecompilerInClassesFAtoFM

	self decompileClassesSelect: [:cn| cn first = $F and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesFNtoFZ
	self decompileClassesSelect: [:cn| cn first = $F and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesGAtoGM
	self decompileClassesSelect: [:cn| cn first = $G and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesGNtoGZ
	self decompileClassesSelect: [:cn| cn first = $G and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesHAtoHM
	self decompileClassesSelect: [:cn| cn first = $H and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesHNtoHZ
	self decompileClassesSelect: [:cn| cn first = $H and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesIAtoIM
	self decompileClassesSelect: [:cn| cn first = $I and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:28'!
testDecompilerInClassesINtoIZ

	self decompileClassesSelect: [:cn| cn first = $I and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesJAtoJM
	self decompileClassesSelect: [:cn| cn first = $J and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesJNtoJZ
	self decompileClassesSelect: [:cn| cn first = $J and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesKAtoKM
	self decompileClassesSelect: [:cn| cn first = $K and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesKNtoKZ
	self decompileClassesSelect: [:cn| cn first = $K and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:37'!
testDecompilerInClassesLAtoLM

	self decompileClassesSelect: [:cn| cn first = $L and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesLNtoLZ
	self decompileClassesSelect: [:cn| cn first = $L and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:51'!
testDecompilerInClassesMAtoMM

	self decompileClassesSelect: [:cn| cn first = $M and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesMNtoMZ
	self decompileClassesSelect: [:cn| cn first = $M and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesNAtoNM
	self decompileClassesSelect: [:cn| cn first = $N and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesNNtoNZ
	self decompileClassesSelect: [:cn| cn first = $N and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:58:57'!
testDecompilerInClassesOAtoOM

	self decompileClassesSelect: [:cn| cn first = $O and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesONtoOZ
	self decompileClassesSelect: [:cn| cn first = $O and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesPAtoPM
	self decompileClassesSelect: [:cn| cn first = $P and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesPNtoPZ
	self decompileClassesSelect: [:cn| cn first = $P and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesQAtoQM
	self decompileClassesSelect: [:cn| cn first = $Q and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesQNtoQZ
	self decompileClassesSelect: [:cn| cn first = $Q and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:04'!
testDecompilerInClassesRAtoRM

	self decompileClassesSelect: [:cn| cn first = $R and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesRNtoRZ
	self decompileClassesSelect: [:cn| cn first = $R and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:11'!
testDecompilerInClassesSAtoSM

	self decompileClassesSelect: [:cn| cn first = $S and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:18'!
testDecompilerInClassesSNtoSZ

	self decompileClassesSelect: [:cn| cn first = $S and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:24'!
testDecompilerInClassesTAtoTM

	self decompileClassesSelect: [:cn| cn first = $T and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesTNtoTZ
	self decompileClassesSelect: [:cn| cn first = $T and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesUAtoUM
	self decompileClassesSelect: [:cn| cn first = $U and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesUNtoUZ
	self decompileClassesSelect: [:cn| cn first = $U and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:30'!
testDecompilerInClassesVAtoVM

	self decompileClassesSelect: [:cn| cn first = $V and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesVNtoVZ
	self decompileClassesSelect: [:cn| cn first = $V and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 9/24/2020 16:59:36'!
testDecompilerInClassesWAtoWM

	self decompileClassesSelect: [:cn| cn first = $W and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesWNtoWZ
	self decompileClassesSelect: [:cn| cn first = $W and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesXAtoXM
	self decompileClassesSelect: [:cn| cn first = $X and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesXNtoXZ
	self decompileClassesSelect: [:cn| cn first = $X and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesYAtoYM
	self decompileClassesSelect: [:cn| cn first = $Y and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesYNtoYZ
	self decompileClassesSelect: [:cn| cn first = $Y and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesZAtoZM
	self decompileClassesSelect: [:cn| cn first = $Z and: [cn second asUppercase <= $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'eem 9/23/2008 11:16'!
testDecompilerInClassesZNtoZZ
	self decompileClassesSelect: [:cn| cn first = $Z and: [cn second asUppercase > $M]]! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:04'!
testRemoteTemp
	| aBlock |
	aBlock := Compiler evaluate: '| x y |  [:a :b | x := a. y := b. x+y]'.
	self shouldnt: [aBlock decompile] raise: Error
	! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'HAW 3/17/2019 07:26:22'!
assert: aBoolean description: aString resumable: resumableBoolean 
	
	aBoolean ifFalse: [
		failures ifNil: [ failures := OrderedCollection new].
		 failures addLast: (thisContext sender tempAt: 1) methodReference].
	
	^super assert: aBoolean description: aString resumable: resumableBoolean 
! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'eem 11/10/2008 15:47'!
failures
	^failures! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:06'!
testMirrorAt
	| stackpBefore stackpAfter array byteArray |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	byteArray := ByteArray with: 1 with: 2 with: 3.
	self assert: (thisContext object: array basicAt: 1) = 1.
	self assert: (thisContext object: byteArray basicAt: 2) = 2.
	thisContext object: array basicAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: byteArray basicAt: 2 put: 222.
	self assert: byteArray asArray = #(1 222 3).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array basicAt: 4] raise: Error.
	self should: [thisContext object: byteArray basicAt: 0] raise: Error.
	self should: [thisContext object: byteArray basicAt: 1 put: -1] raise: Error! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'eem 4/8/2009 19:44'!
testMirrorClass
	| stackpBefore stackpAfter |
	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectClass: Array new) = Array.
	self assert: (thisContext objectClass: 1) = 1 class.
	self assert: (thisContext objectClass: ProtoObject new) = ProtoObject.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:06'!
testMirrorEqEq
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext object: Array new eqeq: Array new) == false.
	self assert: (thisContext object: Array eqeq: Array) == true.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:06'!
testMirrorInstVarAt
	| stackpBefore stackpAfter array point |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	point := Point x: 1 y: 2.
	self assert: (thisContext object: array instVarAt: 1) = 1.
	self assert: (thisContext object: point instVarAt: 2) = 2.
	thisContext object: array instVarAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: point instVarAt: 1 put: 1/2.
	self assert: point = (Point x: 1 / 2 y: 2).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array instVarAt: 4] raise: Error.
	self should: [thisContext object: point instVarAt: 3] raise: Error! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:06'!
testMirrorPerform
	| stackpBefore stackpAfter anInterval |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	anInterval := 1 to: 2.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval) == Array.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval superclass) == Interval.
	self should: [thisContext object: anInterval perform:# species withArguments: #() inClass: Point]
		raise: Error.
	self should: [thisContext object: anInterval perform:# species withArguments: OrderedCollection new inClass: Interval]
		raise: Error.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 12/28/2013 19:06'!
testMirrorSize
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectSize: #(1 2 3)) = 3.
	self assert: (thisContext objectSize: '123') = 3.
	self assert: (thisContext objectSize: nil) = 0.
	self assert: (thisContext objectSize: 1) = 0.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"! !

!ParseNodeEnumeratorTest methodsFor: 'tests about enumeration' stamp: 'RNG 9/27/2020 22:25:05'!
testItEnumeratesAllTheParseNodesPresentInAMethodNode

	| blockNode enumeratedNodes methodNode enumerator tempVarsDeclarationNode tempVarDeclarationNode literalNode messageNode nodesToBeEnumerated returnNode variableNode |
	methodNode := [ Parser parse: 'm1 | arg | ^ arg + 2' class: self class ]
		on: SyntaxErrorNotification 
		do: [ :anError | self fail ].

	enumeratedNodes := Set new.
	enumerator := ParseNodeEnumerator ofBlock: [ :parseNode | enumeratedNodes add: parseNode ].
	methodNode accept: enumerator.
	blockNode := methodNode block.
	tempVarsDeclarationNode := methodNode temporariesDeclaration.
	tempVarDeclarationNode := tempVarsDeclarationNode temporaryDeclarationNodes first.
	returnNode := blockNode statements first.
	messageNode := returnNode expr.
	variableNode := messageNode receiver.
	literalNode := messageNode arguments first.
	nodesToBeEnumerated := {
		methodNode . blockNode . tempVarsDeclarationNode . tempVarDeclarationNode.
		returnNode . messageNode . variableNode . literalNode.
	}.

	self assert: (enumeratedNodes includesAllOf: nodesToBeEnumerated)! !

!ParserTest methodsFor: 'test' stamp: 'HAW 2/29/2020 18:09:57'!
testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat

	| methodNode ranges |
	
	"See Parser>>#variable"
	
	methodNode := self class methodNodeFor: 'm1 |v| ^v'.
	
	ranges := methodNode rangeForNode: methodNode tempNodes first ifAbsent: [ self fail ].
	
	self assert: 2 equals: ranges size.
	self assert: ranges includes: (5 to: 5).
	self assert: ranges includes: (9 to: 9).
	
	
	
	! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 3/13/2020 00:39:16'!
testItGeneratesATempDeclarationNodeForEachTempInABlock

	| methodNode blockNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 [ | a b c | 42 ]'.
	blockNode := methodNode block statements first.
	tempsDeclarationNode := blockNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 3/13/2020 00:38:08'!
testItGeneratesATempDeclarationNodeForEachTempInAMethod

	| methodNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 | a b c | ^a + b + c'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 3/13/2020 00:34:19'!
testItGeneratesAnEmptyTempsDeclarationNodeForAMethodWithoutTemps

	| methodNode tempsDeclarationNode |
	methodNode := self class methodNodeFor: 'm1 ^42'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	
	self assert: tempsDeclarationNode allDeclaredVariableNodes isEmpty.! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:39'!
test1
	"
	self new test1
	"
	| prettyPrinted source |

	source _ 
'sample1: x
	x with: [
		self print.
		self print ].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:39'!
test2
	"
	self new test2
	"
	| prettyPrinted source |

	source _ 
'sample2: x
	x
		print;
		with: [ :a :b :c | | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:40'!
test3
	"
	self new test3
	"
	| prettyPrinted source |

	source _ 
'sample3: x
	x
		print;
		with: [ :a :b :c |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:40'!
test4
	"
	self new test4
	"
	| prettyPrinted source |

	source _ 
'sample4: x
	x
		print;
		with: [ | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:41'!
test5
	"
	self new test5
	"
	| prettyPrinted source |

	source _ 
'sample5: x
	x
		print;
		print;
		print;
		
			with: [
				self print.
				self print ]
			do: [
				self print.
				self print ].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 4/19/2014 17:40'!
test6
	"
	self new test6
	"
	| prettyPrinted source |

	source _ 
'sample6: x
	x
		with: [
			self print.
			self print ]
		do: [
			self print.
			self print ].'.

	prettyPrinted _ Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: source equals: prettyPrinted! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 10/19/2020 16:07:48'!
test01isImplicitSelfReturnInReturnsTrueWithMethodWithOutAnySourceCode

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithImplicitReturn) methodNode.
	returnNode := methodNode block statements first.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 10/19/2020 16:08:15'!
test02isImplicitSelfReturnInReturnsTrueWithMethodReferencingSelf

	| methodNode returnNode |
	
	methodNode := (self class >> #methodReferencingSelfWithImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 10/19/2020 16:09:20'!
test03isImplicitSelfReturnInReturnsFalseWithMethodReturningSelf

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithoutImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self deny: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 10/19/2020 16:06:04'!
methodReferencingSelfWithImplicitReturn

	self yourself 
	
	! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 10/19/2020 16:00:38'!
methodWithImplicitReturn
	
	! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 10/19/2020 16:08:38'!
methodWithoutImplicitReturn
	
	^self! !

!ReturnNodeTest methodsFor: 'test support' stamp: 'HAW 10/19/2020 16:03:45'!
returnNodeOf: methodNode
	
	methodNode nodesDo: [:node | node isReturn ifTrue: [^node]].
	
	self error: 'No return node found'! !

!ScannerTest methodsFor: 'testing' stamp: 'jmv 9/5/2016 20:48:53'!
testLiteralSymbols

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | Scanner isLiteralSymbol: (String with: char) asSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | Scanner isLiteralSymbol: str asSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
		
	((32 to: 94), (96 to: 126) collect: [:ascii | Character numericValue: ascii]) ,
	#(':x:yourself' '::' 'x:yourself' '123' 'x0:x1:x2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '-' '++' '+' '+/-' '-/+' '<|>' '#x' '()' '[]' '{}' '')
		do: [:str |
			self assert: (Compiler evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion']! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'RNG 5/8/2020 21:02:35'!
test01TryingToTrimAnAlreadyTrimmedIntervalThatRepresentsASmalltalkExpressionDoesNotChangeTheOriginalInterval
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '3+4'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: originalInterval equals: trimmedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'RNG 5/8/2020 21:02:27'!
test02StartingAndEndingSeparatorsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '	3+4 '.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: (2 to: 4) asSourceCodeInterval equals: trimmedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'RNG 5/8/2020 21:02:09'!
test03StartingAndEndingDotsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '...3+4..'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: (4 to: 6) asSourceCodeInterval equals: trimmedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'RNG 5/8/2020 21:01:42'!
test04GroupsOfParenthesesAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '((3+4))'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: (3 to: 5) asSourceCodeInterval equals: trimmedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'RNG 5/8/2020 21:01:13'!
test05GroupsOfParenthesesIncludingSeparatorsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := ' (	(3+4)
	)'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: (5 to: 7) asSourceCodeInterval equals: trimmedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - equality' stamp: 'RNG 5/27/2020 23:59:37'!
test11AsSourceCodeIntervalMessageDoesNotCreateANewSourceCodeIntervalInstance

	| interval |
	interval := (1 to: 10) asSourceCodeInterval.
	
	self assert: interval == interval asSourceCodeInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'RNG 5/9/2020 15:41:07'!
test06AnIntervalThatDoesNotHaveCharactersToExpandRemainsTheSame

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3+4 factorial'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: originalInterval equals: expandedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'RNG 5/9/2020 15:41:07'!
test07AnIntervalEnclosedByParenthesesCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '(3+4 factorial)'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: (1 to: sourceCode size) equals: expandedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'RNG 5/9/2020 15:41:07'!
test08AnIntervalWithParenthesesOnJustOneSideCannotBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3 + 4 factorial)'.
	originalInterval := (1 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: originalInterval equals: expandedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'RNG 5/9/2020 15:41:07'!
test09AnIntervalEnclosedByBackticksCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`3 + 4 factorial`'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: (1 to: sourceCode size) equals: expandedInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'RNG 5/9/2020 15:41:07'!
test10AnIntervalEnclosedMultipleBackticksAndParenthesesCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`((3 + 4 factorial))`'.
	originalInterval := (4 to: sourceCode size - 3) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: (1 to: sourceCode size) equals: expandedInterval! !

!FileIOAccessorTest methodsFor: 'private' stamp: 'jmv 5/31/2016 10:30'!
defaultDirectoryPath
	^DirectoryEntry currentDirectory pathName! !

!FileIOAccessorTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:48'!
testDirectoryExists
	"FileIOAccessorTest debug: #testDirectoryExists"
	| subDirString dirString |
	subDirString := 99999 atRandom asString.
	dirString := self defaultDirectoryPath, FileIOAccessor default slash, subDirString.

	FileIOAccessor default createDirectory: dirString.

	self should: [ dirString asDirectoryEntry exists ].

	FileIOAccessor default deleteDirectory: dirString.

	self shouldnt: [ dirString asDirectoryEntry exists ].! !

!FileManTest methodsFor: 'private' stamp: 'jmv 5/31/2016 10:28'!
directoryEntryForTest
	^'./fmTestDir' asDirectoryEntry! !

!FileManTest methodsFor: 'private' stamp: 'jmv 5/31/2016 10:28'!
randomFileName
	^100000 atRandom asString, '.fmtst'! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testAbsolutePath
	"FileManTest debug: #testAbsolutePath"
	| dirEntry dirEntry1 dirEntry2 dirEntry3 dirEntry4 |
	dirEntry := '/' asDirectoryEntry.
	self should: [dirEntry = ':' asDirectoryEntry].
	self should: [dirEntry = '\' asDirectoryEntry].
	
	dirEntry1 := '/temp/' asDirectoryEntry.
	self should: [dirEntry1 = ':temp' asDirectoryEntry].
	self should: [dirEntry1 = '\temp' asDirectoryEntry].

	dirEntry2 := '/temp/a' asDirectoryEntry.
	self should: [dirEntry2 = ':temp:a' asDirectoryEntry].
	self should: [dirEntry2 = '\temp\a' asDirectoryEntry].

	dirEntry3 := 'C:/temp/b' asDirectoryEntry.
	self should: [dirEntry3 = 'C:\temp\b' asDirectoryEntry].
	self should: [dirEntry3 = 'C::temp:b' asDirectoryEntry].

	"Platform specific path tests"
	FileIOAccessor default onMacClassic ifTrue: [
	dirEntry4 := 'Macintosh HD:tmp' asDirectoryEntry.
	self should: [dirEntry4 = 'Macintosh HD/tmp' asDirectoryEntry].
	self should: [dirEntry4 = 'Macintosh HD\tmp' asDirectoryEntry].
	].
	
	
	
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testAtPut
	"FileManTest debug: #testAtPut" 
	| dir bytes |
	dir := self directoryEntryForTest.
	dir at: 'test1' put: 'Hello'.
	self should: [(dir at: 'test1') = 'Hello'].
	self should: [dir includesKey: 'test1'].

	bytes := #(1 2 3 4 5 6) asByteArray.
	dir binaryAt: 'test2' put: bytes.
	self should: [(dir binaryAt: 'test2') = bytes].
	self should: [dir includesKey: 'test2'].

	dir removeKey: 'test1'.

	self shouldnt: [dir includesKey: 'test1'].

	dir recursiveDelete.
	self should: [dir exists not]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testConcatenation
	"
	FileManTest debug: #testConcatenation
	"
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa/bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg').
	dir at: 'test1' put: 'RecursiveDeleted!!'.

	self assert: dir name = 'ggg'.
	self assert: dir parent name = 'fff'.
	self assert: dir parent parent name = 'eee'.
	self assert: dir parent parent parent name = 'ddd'.
	self assert: dir parent parent parent parent name = 'ccc'.
	self assert: dir parent parent parent parent parent name = 'bbb'.
	self assert: dir parent parent parent parent parent parent name = 'aaa'.

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testCopy
	"FileManTest debug: #testCopy" 
	| file1 file2 |
	file1 := self randomFileName asFileEntry.
	file2 := file1 parent // self randomFileName.

	file1 fileContents: 'This is a test'.

"	self should: [file2 fileContents isEmpty]."
	self should: [file2 exists not].

	file1 copyTo: file2 pathName.

	self should: [file2 fileContents = 'This is a test'].

	file1 delete.
	file2 delete.
	self should: [file1 exists not].
	self should: [file2 exists not]
	
	
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 6/1/2016 17:17'!
testDefaultDirectory
	"
	FileManTest debug: #testDefaultDirectory
	"
	
	| pathComponents |

	"
	See #asAbsolutePathName. See #isAbsolutePathName
	self assert: '' asDirectoryEntry =  DirectoryEntry currentDirectory.
	"

	pathComponents := '' asDirectoryEntry pathComponents.
	self assert: pathComponents = DirectoryEntry currentDirectory pathComponents! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testFileContents
	"FileManTest debug: #testFileContents" 
	| file1 file2 bytes |
	file1 := self randomFileName asFileEntry.
	file1 fileContents: 'This is a test'.
	self should: [file1 fileContents = 'This is a test'].
	file1 delete.
	self should: [file1 exists not].

	file2 := self randomFileName asFileEntry.
	bytes := #(1 2 3 4 5 6) asByteArray.
	file2 fileContents: bytes.
	self should: [file2 fileContents = bytes asString].
	self should: [file2 binaryContents = bytes].
	file2 delete.
	self should: [file2 exists not]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:49'!
testIsAbsolutePathName
	"
	FileManTest debug: #testIsAbsolutePathName
	"
	self assert: '/' isAbsolutePathName.
	self assert: '/temp/' isAbsolutePathName.
	self assert: '/temp/a' isAbsolutePathName.
	Smalltalk platformName = 'Win32' ifTrue: [
		self assert: 'C:/temp/b' isAbsolutePathName ].
	FileIOAccessor default onMacClassic ifTrue: [
		self assert: 'Macintosh HD/tmp' isAbsolutePathName ].
	
	self deny: './' isAbsolutePathName.
	self deny: '../' isAbsolutePathName.
	self deny: 'afile' isAbsolutePathName.! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testIsRelativePathName
	"
	FileManTest debug: #testIsRelativePathName
	"
	self assert: './' isRelativePathName.
	self assert: '../' isRelativePathName.
"	self assert: 'afile' isRelativePathName."
	self deny: '/' isRelativePathName.
	self deny: '/temp/' isRelativePathName.
	self deny: '/temp/a' isRelativePathName.
	self deny: 'C:/temp/b' isRelativePathName.
	self deny: 'Macintosh HD/tmp' isRelativePathName.! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testPathComponents
	"
	FileManTest debug: #testPathComponents
	"
	| pathComponents |

	pathComponents := './aaa/bbb\ccc:ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd').

	pathComponents := '/aaa/bbb\ccc:ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd').

	pathComponents := 'aaa/bbb\ccc:ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd')! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testPipe
	"FileManTest debug: #testPipe" 
	| reverseFilter file1 file2 file3 |

	reverseFilter := [:in :out | out nextPutAll: (in upToEnd reverse)].

	file1 := self randomFileName asFileEntry.
	file2 := self randomFileName asFileEntry.
	file3 := self randomFileName asFileEntry.

	file1 fileContents: 'This is a pipe test'.

	file1 pipe: reverseFilter to: file2 pathName.

	self should: [('.' asDirectoryEntry at: file1 name) = 'This is a pipe test'].	
	self should: [(file2 fileContents) = 'tset epip a si sihT'].	
"	self should: [(file3 fileContents) isEmpty]."
	self should: [file3 exists not].

	file2 pipe: reverseFilter to: file3 pathName.
	self should: [(file3 fileContents) = 'This is a pipe test'].	

	file1 delete.
	file2 delete.
	file3 delete.
	self should: [file1 exists not].
	self should: [file2 exists not].
	self should: [file3 exists not]
	
	
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testRecursiveDelete
	"FileManTest debug: #testRecursiveDelete" 
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg').
	dir at: 'test1' put: 'RecursiveDelete!!'.
	self should: [(dir at: 'test1') = 'RecursiveDelete!!'].

	dir removeKey: 'test1'.

	self shouldnt: [(dir // 'test1') exists].

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].

	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testRefresh
	"FileManTest debug: #testRefresh" 
	| file1 |
	file1 := self randomFileName asFileEntry.

	file1 fileContents: '1234567890'.
	self should: [file1 fileSize = 10].

	file1 fileContents: '123'.
	self should: [file1 fileSize = 3].
	

	file1 delete.
	self should: [file1 exists not].
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:50'!
testRename
	"FileManTest debug: #testRename" 
	| file1 |
	file1 := self randomFileName asFileEntry.
	file1 fileContents: 'ToBeRenamed'.

	self shouldnt: [file1 name = 'newName1'].

	file1 rename: 'newName1'.

	self should: [file1 name = 'newName1'].
	self should: [file1 exists].

	self should: [file1 fileContents = 'ToBeRenamed'].

	file1 delete.
	self should: [file1 exists not].
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 12:02'!
testRoot
	"FileManTest debug: #testRoot"
	| root |
	root := DirectoryEntry roots first.
	self should: [root pathComponents isEmpty].
	FileIOAccessor default onUnix ifTrue: [
		self should: [root = '\' asDirectoryEntry]. 
		self should: [root = ':' asDirectoryEntry]. 
		self should: [root = '/' asDirectoryEntry]]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 11:19'!
testStream
	"FileManTest debug: #testStream" 
	| file1 contents formerContents allContents |
	file1 := self randomFileName asFileEntry.
	file1 writeStreamDo: [:str | str nextPutAll: 'HELLO!!'].
	contents := file1 readStreamDo: [:str | str upToEnd].
	self should: [contents = 'HELLO!!'].

	file1 appendStreamDo: [:str | str nextPutAll: 'AGAIN!!'].

	formerContents := file1 readStreamDo: [:str | str upTo:$!!].
	self should: [formerContents = 'HELLO'].

	allContents := file1 readStreamDo: [:str | str upToEnd].
	self should: [allContents = 'HELLO!!AGAIN!!'].

	file1 delete.
	self should: [file1 exists not].
	! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/22/2011 23:57'!
testAddressRange
	"Test source pointer to file position address translation across the full address range"
	
	| sf |
	sf := CuisSourceFileArray new.
	(16r1000000 to: 16r4FFFFFF by: 811) do: [:e | | i a p |
		i := sf fileIndexFromSourcePointer: e.
		p := sf filePositionFromSourcePointer: e.
		a := sf sourcePointerFromFileIndex: i andPosition: p.
		self assert: a = e]
! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/23/2011 00:08'!
testChangesFileAddressRange
	"Test file position to source pointer address translation for the changes file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 i p |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 2.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 2 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r3FFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r4FFFFFF)]


! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/23/2011 00:06'!
testFileIndexFromSourcePointer
	"Test derivation of file index for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file mapping"
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1FFFFFF).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2FFFFFF).
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	(16r2000000 to: 16r2FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	"changes file mapping"
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3FFFFFF).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4FFFFFF).
	(16r3000000 to: 16r3FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)].
	(16r4000000 to: 16r4FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)]


! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/23/2011 00:09'!
testFilePositionFromSourcePointer
	"Test derivation of file position for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r1000000).
	"changes file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r3000000).! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/23/2011 00:10'!
testSourcePointerFromFileIndexAndPosition
	"Test valid input ranges"

	| sf |
	sf := CuisSourceFileArray new.
	self should: [sf sourcePointerFromFileIndex: 0 andPosition: 0] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 0] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 2 andPosition: 0] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 0 andPosition: 3] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 1 andPosition: -1] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 16r2000000] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 3 andPosition: 0] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 4 andPosition: 0] raise: Error.
	
	self assert: 16r1000000 = (sf sourcePointerFromFileIndex: 1 andPosition: 0).
	self assert: 16r3000000 = (sf sourcePointerFromFileIndex: 2 andPosition: 0).! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 5/23/2011 00:11'!
testSourcesFileAddressRange
	"Test file position to source pointer address translation for the sources file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 p i |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 1.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 1 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r1FFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r2FFFFFF)]

! !

!SystemConsistencyTest methodsFor: 'testing' stamp: 'jmv 5/7/2012 09:35'!
testMethodsWithUnboundGlobals
	"
	SystemConsistencyTest new testMethodsWithUnboundGlobals
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 6/21/2011 22:05'!
assert: subject isEquivalentToDictionary: controlGroup
	"Test #size, #do:, "
	| count subjectAsDictionary someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup keysDo: [ :each |
		self assert: (subject includesKey: each) ].
	subject keysDo: [ :each |
		self assert: (controlGroup includesKey: each) ].
	controlGroup keysAndValuesDo: [ :k :v |
		self assert: (subject at: k) = v ].
	subject keysAndValuesDo: [ :k :v |
		self assert: (controlGroup at: k) = v ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includesKey: someRandomString)
		= (controlGroup includesKey: someRandomString).
	subjectAsDictionary _ Dictionary new.
	count _ 0.
	subject keysAndValuesDo: [ :k :v |
		subjectAsDictionary at: k put: v.
		count _ count + 1 ].
	self assert: subjectAsDictionary size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsDictionary = controlGroup.! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 6/15/2011 09:58'!
assert: subject isEquivalentToSet: controlGroup
	"Test #size, #do:, "
	| count subjectAsSet someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup do: [ :each |
		self assert: (subject includes: each) ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includes: someRandomString)
		= (controlGroup includes: someRandomString).
	subjectAsSet _ Set new.
	count _ 0.
	subject do: [ :element |
		subjectAsSet add: element.
		count _ count + 1 ].
	self assert: subjectAsSet size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsSet = controlGroup.! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 6/22/2011 12:51'!
assert: subject prefixSelection: aString isEquivalentToSet: controlGroup
	| selection1 selection2 |
	selection1 _ OrderedCollection new.
	subject forPrefix: aString keysAndValuesDo: [ :k :v |
		selection1 add: k ].
	selection2 _ (controlGroup select: [ :each |
		aString isEmpty or: [
			each asLowercase asUnaccented beginsWith: aString asLowercase asUnaccented]]) asOrderedCollection 
				sort: [ :a :b | a asLowercase asUnaccented < b asLowercase asUnaccented ].
	self assert: selection1 = selection2.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/21/2011 22:14'!
testAllMesssagesLikeDicionary
	"
	TrieTest new testAllMesssagesLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].
	self assert: subject isEquivalentToDictionary: controlGroup.

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/21/2011 22:01'!
testAllMesssagesLikeSet
	"
	TrieTest new testAllMesssagesLikeSet
	"
	| subject  controlGroup allMessages copy|
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].
	self assert: subject isEquivalentToSet: controlGroup.

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:38:28'!
testIncludesReturnsFalseForObjectsThatAreNotString
	
	self deny: ((Trie with: 'hello') includes: 1)
	! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:38:51'!
testIncludesReturnsFalseForStringsNotIncluded
	
	self deny: ((Trie with: 'hello') includes: 'bye')
	! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:39:09'!
testIncludesReturnsTrueForIncludedStrings
	
	self assert: ((Trie with: 'hello') includes: 'hello')
	! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 4/4/2019 08:18:55'!
testKeyNotFoundSignalTheRightMessage

	self 
		should: [ Trie new errorKeyNotFound ]
		raise: Error
		withMessageText: Dictionary keyNotFoundErrorDescription ! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/22/2011 12:30'!
testPrefixIteration
	"
	TrieTest new testPrefixIteration
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'cat'.
	controlGroup add: 'cat'.
	subject add: 'cart'.
	controlGroup add: 'cart'.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	subject add: 'ñandues'.
	controlGroup add: 'ñandues'.
	subject add: 'ÑANDÚSES'.
	controlGroup add: 'ÑANDÚSES'.
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñan' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandú' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'Ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ÑANDÚ' isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/22/2011 12:50'!
testPrefixIteration2
	"
	TrieTest new testPrefixIteration2
	"
	| subject  controlGroup allMsg toAvoid |
	subject _ Trie new.
	controlGroup _ Set new.

	allMsg _ Smalltalk allImplementedMessages.
	toAvoid _ (allMsg asArray collect: [ :each | each asLowercase asUnaccented ]) asBag.
	allMsg do: [ :symbol |
		(toAvoid occurrencesOf: symbol) = 1 ifTrue: [
			subject add: symbol.
			controlGroup add: symbol ]].
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'a' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:p' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:pu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:nonExistant:' isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/23/2011 11:02'!
testRemoveLikeDicionary
	"
	TrieTest new testRemoveLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.

	subject at: 'doNotRemove' put: 'this'.
	controlGroup at: 'doNotRemove' put: 'this'.
	
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].

	subject at: 'doNotRemove2' put: 'this2'.
	controlGroup at: 'doNotRemove2' put: 'this2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	
	allMessages do: [ :symbol |
		subject removeKey: symbol.
		controlGroup removeKey: symbol ].

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2.
	self assert: subject isEmpty = controlGroup isEmpty.

	subject removeKey: 'doNotRemove'.
	controlGroup removeKey: 'doNotRemove'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject removeKey: 'doNotRemove2'.
	controlGroup removeKey: 'doNotRemove2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/22/2011 15:54'!
testRemoveLikeSet
	"
	TrieTest new testRemoveLikeSet
	"
	| subject  controlGroup allMessages copy |
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.

	subject add: 'doNotRemove'.
	controlGroup add: 'doNotRemove'.
	
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].

	subject add: 'doNotRemove2'.
	controlGroup add: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	
	allMessages do: [ :symbol |
		subject remove: symbol.
		controlGroup remove: symbol ].

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.

	subject remove: 'doNotRemove'.
	controlGroup remove: 'doNotRemove'.

	self assert: subject isEquivalentToSet: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject remove: 'doNotRemove2'.
	controlGroup remove: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 6/22/2011 15:42'!
testSetProtocol
	"
	TrieTest new testSetProtocol
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.
	self assert: subject isEquivalentToSet: controlGroup.
	
	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cat'.
	controlGroup add: 'cat'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cart'.
	controlGroup add: 'cart'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandu'.
	controlGroup add: 'ñandu'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ÑANDÚ'.
	controlGroup add: 'ÑANDÚ'.
	self assert: subject isEquivalentToSet: controlGroup.! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 9/25/2012 22:42'!
testDiskProxy
	"
	ReferenceStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 12/6/2011 08:19'!
testSortedCollection
	"
	ReferenceStreamTest new testSortedCollection
	"
	| newInstance oldInstance |
	oldInstance _ SortedCollection sortBlock: [ :a :b | a printString < b printString ].
	oldInstance add: 'hi'; add: 'there'; add: 'you'; add: 'all'.
	newInstance _ ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	self assert: newInstance asArray = oldInstance asArray! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 8/21/2012 17:02'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	ReferenceStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ ReferenceStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	window delete! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 9/25/2012 22:42'!
testDiskProxy
	"
	SmartRefStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 12/6/2011 08:34'!
testSortedCollection
	"
	SmartRefStreamTest new testSortedCollection
	"
	| newInstance oldInstance |
	oldInstance _ SortedCollection sortBlock: [ :a :b | a printString < b printString ].
	oldInstance add: 'hi'; add: 'there'; add: 'you'; add: 'all'.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	self assert: newInstance asArray = oldInstance asArray! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 8/21/2012 17:00'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	SmartRefStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ SmartRefStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	window delete! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:40'!
testAllAlphasRgbAdd
	"self run: #testAllAlphasRgbAdd"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 20.	"rgbAdd"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = (destAlpha + sourceAlpha min: 255)
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbAdd'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:40'!
testAllAlphasRgbMax
	"self run: #testAllAlphasRgbMax"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 27.	"rgbMax"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = (destAlpha max: sourceAlpha)
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbMax'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:40'!
testAllAlphasRgbMin
	"self run: #testAllAlphasRgbMin"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 28.	"rgbMin"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = (destAlpha min: sourceAlpha)
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbMin'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:39'!
testAllAlphasRgbMinInvert
	"self run: #testAllAlphasRgbMinInvert"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 29.	"rgbMinInvert"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = (destAlpha min: 255-sourceAlpha)
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbMinInvert'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:39'!
testAllAlphasRgbMul
	"self run: #testAllAlphasRgbMul"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 37.	"rgbMul"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = ((destAlpha+1) * (sourceAlpha+1)- 1 // 256)
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbMul'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 3/11/2010 08:39'!
testAllAlphasRgbSub
	"self run: #testAllAlphasRgbSub"
	| sourceForm destForm blt correctAlphas |
     correctAlphas _ 0.
     0  to: 255 do: [:sourceAlpha |
         sourceForm _ Form extent: 1 @ 1 depth: 32.
         sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
         0 to: 255 do: [:destAlpha |
             destForm _ Form extent: 1 @ 1 depth: 32.
             destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
              blt _ BitBlt new.
              blt sourceForm: sourceForm.
              blt sourceOrigin: 0 @ 0.
              blt setDestForm: destForm.
              blt destOrigin: 0 @ 0.
              blt combinationRule: 21.	"rgbSub"
              blt copyBits.
              correctAlphas _ correctAlphas
                + (((blt destForm bits at: 1) digitAt: 4) = (destAlpha - sourceAlpha) abs
                         ifTrue: [1]
                         ifFalse: [0])
      ]].
     self assert: correctAlphas = 65536 description: 'Some incorrect alpha values computed for BitBlt rule rgbSub'! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 7/12/2015 22:27'!
testAlphaCompositing
	"
	BitBltTest new testAlphaCompositing
	"

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: (Color r: 0 g: 0 b: 1).
		mixColor := Color r: 1 g: 0 b: 0 alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb := BitBlt toForm: f1.
		bb sourceForm: f2.
		bb combinationRule: Form blend.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: (result blue - (1.0 - mixColor alpha)) abs < eps.
		self assert: result alpha = 1.0.
	].! !

!BitBltTest methodsFor: 'bugs' stamp: 'jmv 7/12/2015 22:27'!
testAlphaCompositing2
	"
	BitBltTest new testAlphaCompositing2
	"

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: Color transparent.
		mixColor := Color r: 1 g: 0 b: 0 alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb := BitBlt toForm: f1.
		bb sourceForm: f2.
		bb combinationRule: Form blend.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: result alpha = mixColor alpha.
	].! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:20'!
test16bpp
	| form n read |
	form _ JpegTest lenaColor64 asFormOfDepth: 16.
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:16'!
test16bpp61
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:13'!
test16bpp62
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:10'!
test16bpp63
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:07'!
test16bppLE
	| form n read |
	form _ JpegTest lenaColor64 asFormOfDepth: -16.
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:04'!
test16bppLE61
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:59:00'!
test16bppLE62
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:57'!
test16bppLE63
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:53'!
test32bpp
	| form n read |
	form _ JpegTest lenaColor64.
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:50'!
test32bpp63
	| form n read |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:47'!
test32bppLE63
	| form n read |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:44'!
testBA16bpp
	| form n read bytes |
	form _ JpegTest lenaColor64 asFormOfDepth: 16.
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:41'!
testBA16bpp61
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:37'!
testBA16bpp62
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:34'!
testBA16bpp63
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:31'!
testBA16bppLE
	| form n read bytes |
	form _ JpegTest lenaColor64 asFormOfDepth: -16.
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:28'!
testBA16bppLE61
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:25'!
testBA16bppLE62
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:20'!
testBA16bppLE63
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:16'!
testBA32bpp
	| form n read bytes |
	form _ JpegTest lenaColor64.
	self assert: form depth = 32.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:07'!
testBA32bpp63
	| form n read bytes |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:03'!
testGray
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm.
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:58:01'!
testGray61
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:57'!
testGray62
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:54'!
testGray63
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:48'!
testGrayBA
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm.
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:44'!
testGrayBA61
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:40'!
testGrayBA62
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest methodsFor: 'testing' stamp: 'jmv 2/21/2019 16:57:35'!
testGrayBA63
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8! !

!JpegTest class methodsFor: 'test examples' stamp: 'jmv 2/21/2019 16:55:22'!
lenaColor64
	"
	('LenaColor64.bmp' asFileEntry readStreamDo: [ :strm | strm binary. Base64MimeConverter mimeEncode: strm ]) upToEnd
	JpegTest lenaColor64 display
	"
	^Form fromBinaryStream:
		'Qk02MAAAAAAAADYAAAAoAAAAQAAAAEAAAAABABgAAAAAAAAwAADwCgAA8AoAAAAAAAAAAAAA
QWCvUXXHUHfGRGi/OTZkIB8tHx0qHx0oISArHh4pIyEtIR8vIB4qHx0pJyQxJB8vNCo9TjpI
V0NOHR0rHh4qHR4qHR0xISlbJTx/Kzl7IidFMUSPNU6lOlOsPVWtPlaxQFm3Qly4QFu4QFq4
PVayOlWvP1myRWC3RmO3Um6/XnnJZIHNhJjbmKXhnqvmo67nmKXcKC9TLzlhKzJaLC5RJihG
ISNAR2ioYJbdQmaxJSxTHiA5IiNCKCtPLC1RJyZDQl+uUXTETnTDRmrAOjtrIyEwIB4qIB4q
JiQyJCIrJCIvJSExIB4qHRwoJyQyMSg1XkZSUkJUT0FmHRwqHh4uHR0tHR4sHyE6JjJzLD+K
Jjd6JSlIN06hOlOsPlSsP1a0Qlu4Q1y4Pli3PVe1P1q0QVuzQ1y1R2O6UGrAV3PEZH/OcYrR
h53dm6rln63moq/naG+gLDZdLzhkJitSJCVGHh80HR4wSG+vSoDJY5TdSnrJISZTJCVEJSdJ
KzBZKy5VR2S0UHPFUnbIQ2a7MzZzJyM2Hx0qIB4oJCAtJiMvJSItISAuHhwoJiQwNCw4UzxK
RTQ+JiEsLiY4KSU1HB4uHh8tHh4uHh4wHyVOLT6HL0KLKDJqLzluOVGnO1CpPle0QFq1Q1y5
OlSxPVexP1m0Q164SWO7SmW7U27AVnPDbIXPfJPWkaXknKvloa7po7HpOUh1KzJVIyhKIidK
KCtLKS1MMTlgQl6cRXK2VIHEcajtNFKeJStTIihWIyhQMDRgSWm8UnbJVXrKQ2a6Mjl1JiQ6
IB8pJCM0Hx0pJCAuIyEsIyIwJiMyJyMwJyEyRDVALCg5STlGLCQ2X01WRT5PHR8zHyA0HiAx
Hx43KTd1L0OTL0SWKSlTOE2gPlSrP1ezP1q1QVq1NlOtPFiyQFu1RF63SmW7Tmq/VnHBX3vJ
cYrRgZfZk6blorHvo7Ltoa3mMUJ6LDRaMTplOkdyQU99RFJ/R1iHR2OiOlaaSW2vfrHuS37L
Jzh5JitaJjBiJipRTGy/U3fJUXbHO1+0QkZ1ISEwHx4pHhwnJB8vKCUwIB4rIB8qIh8qJSIu
MiY2Oy88WkRLXkhQSTdEPTU/Hh4oHiI2HRs0HCA2HR0yICRPLkCHNUmbJCxfNkOHPlOrP1ay
QFq2P1e0O1WwO1ayQFu0RWC4Sma9U27CXHbHZYDMeZPXiZ7glqjnna7robDrY2ubRl6ZSluH
SluKRlmISluKSVmGR1aCQVaPOFafRnO+fq7vZZvkMEqTJzd2KzRnISlcTGy+U3bGUHG+RmGx
Rj9tISArHx8pIyEuJiMzMi06KSY2JCEuIyEvIB0pLyc0HxonSjdEZU9fU0RUUkZUUktfKylM
NDtvJyM5HR4zHyE7KDh8M0iaLjt7LDZtOE+mPlWuP1i1PFaxOlOsPli0RV22SmO7TGi9Um7C
X3nJaobRgJjbj6Tjmazrna/qnq7oSk9zR1yaU2maTF6NS16NSFmJRlmHRFOAQVOGUnm/canx
d6rsc6frPF6sJjt+JjNqLTt2TW7AVHnNVHrMQ2S6MDdnIiEuIR4rIR8rIiEqRC9BJyQyIB0n
ISAuIB4pLSk2SztJPCo5YEtUVU1lfWiCV1BgFhgoIiNAHB4yICE1ICE1KjFcMUaXKTqAKS1b
OE6kPFOuPVWwP1ewQFaxQ1y2SGC4S2S7VW/CWHPGZ4LRc47XhZ3gkKfnobDtoLDoprfvIB80
Q12fTGOZSl6RSV2RRViIRViIRFKAO06BUHK0gLTweqvvfq/uToDMLECBKDNsKjp1Tmy9VnvP
UnbKP2O4SUp6HiApIiArHx0pICAqNCgyLSo7KCY0JCAuNC4+MSEzTTdHSDZGY0taaFFkGhoq
X1x1HBwpIydFHhwvHh4xHyE0HiE9KjmBJzZ4JyhLMkiePVOrP1WxPVWxPFavQ123R2C3TGa9
V3LEWnfIZ4POdI7WhJ/kkqnqobLsprXsbHqpICE4Q1qZSV+VSl6VR1ySSFuLR1eGQlF/PEpz
P1iUe6nsa6Dpg7Xwap7iME+VKDNmKTZuTWu9UnbIVXfKPmK4Sk2LExgkISAnKCc3Hx8nJB8u
PS9BJCAuOC8/LSQxOCs7SzZFQjBAZk9bcV1uQzxQT1FtHR8kIyRBHyAzHh0vHyA2ICI3LDdx
KzZyKi1KMkSYPFGoPFKsP1WsQVmxQ1uzSWK6T2u+VnLCYn7NaofTc5LcgqDljqfon7HtpbLn
OTtaISM7Lz9vS1+TTWGYSl+URliKQlOEQ1ODRVOAN0l+QmatW4fNfq/sdafoUYDHJSxfJC1i
S2m6VXfLTXLGQl63bG2TJCE5JCExJCEwIB8wJyMzMCc4IB4rJiIuSThIMSQ0QzFAWURVZlNg
RTZHYl2DPTpLHR8mKCpHHB4uICAxIiM5Jic+KTFjIihdMDFZO0yYNkmYP1atQliuQViuRl20
UGq/Vm/BYnzLY3/KaIjTdZXehKPoj6rpoLLsWl+DHR83IyZDPUh2S2CTS2CWR12SSFmKRliK
RlWFQk95QE13Mk6RPF2jfq/ue6zrZ5fYLUJ7JytTT23AUXLGTnLIQV61XWCRPEBpISErKykz
LSg8OzBJOi9DKiMuMio+SjtLMCIyRzRDalJgaFJZUVFwOi1FKChFHR4pJShFHRwuHh4wHyEz
Hh40JipRIiNBLTNiM0ONNkePPlGfQ1muQVesQ1qwT2a4WHLBZH/Jb4vTdJLYfZvgh6LkkKfj
eoCjIyc+HR46JCZIR1F3RVuTSl+SSVyPSFqNRFKCRVGAP0tzNj9pMUqHLkSBa5rcd6rqcaHh
TXK1ISE8UG3AU3LHVnTFU23AKipTQDxVIB8pJyQyLyU3KiQ0NCw/KyQyVkBQUz9NVkROSzlH
SjNAUz1STUJZOzFIICA+Hx8tISQ+Hx8wHx8zHh4wHyA1HyA/IyU+KC9gNkWMOkuYPE6bP1Sk
RFmsRlutR12uVW68cIjOe5XWgZvdh5/fi5/ZR01qIiE6IylIIyRFJSlNPkBhP1WTSlyORFeM
P058OUp/Mj5rLDVfKTBWMUBuKjNdV3m7b6TreKbnZJPXPlqZUG7AVHPFUHTHQFqqYWeUKiFD
JydEKSM1KSU4KyY0Ix8sVz1KXURPJyExb1RbUT1LZExRQz1RXlJrHx4rISE6Hx8yISQ7Hx8w
HR4wHh4xICE2HyA8HSA6KTJkNkWTOEmYPU+fPFKdSF2vT2OySV2pUmaxaoXEf5fVd4i5TVZ5
ISEuHyAzISM5ICVEJihKISZIKypEQFKQOUl7MUF0M0B0LDloMTtmND9pO0VvQEhvQklrZYjI
canvb6Lmc6PkYYzTUW/BVXbIT3PIPFquIytXTERnNjlRKSE4KCQzKSM0IRwocVFPPTA+QTA/
YUlRY0tdW1ByST1UHR4pHR4rICI3ISI2IiM6Hh0wHh4wHh40ICI5ICI9HiJDICRGICZTNT16
N0aCOEqLTFusVmm7WGy7XG29XmqlMjdgHxw1ISAzIyI5ICE1IiM6LzVWJilLJCdGIyA9Qlyd
MD5qNkV3QEt/RlWDSlmFSluISlqFRlN6QUpvYHyzhLXyea3sdqfpcaLoUW/AVHbIUXTGPVqs
KC1gKipSLClGNi5GJB8tKCExKSEwc1RVRzRCZEhQWUJMX1J6KClaLjBUICAyIiAxIB8xICE2
IiE4Hx40Hh8xJCY8IyQ8HiA6HyJGHyJLJzRxNEWZPVGmQVeuTGK3X3PGXG/AXnC/UGGqLThw
HR00IB8xHx83ICA5IiI5MTZbJytNHiE8IyM7WXCvRlmOTWKVTWGQT2GRS1+QT2GQTV2HR1WB
TFZ9T2WWksD3fLHvfrHwg7DuTm2/VHjMVHjMPVywIydaODZdNTBTLCU3JCAwLCcyKSMwmXJo
VT5CW0JSSjZGRj5iPT1me4rgISdKHx8uHyAxHB0xHh0zICEyHyA4IyQ+ICM+JCZGIipdLjl7
MkKQOEyiQVWsRVqxSFyzTWK4VGq/VWy/T2OyQFOfIB42Hh4wHh8zHyA4HR41KzFSJypOJitF
IiM9W3GvSV6UUGicUGaYTGGUU2eXUGOUTFyKTVuIRlR+QVSBj7v2irv3h7TxhLDuUG/DVXnO
VnjNS2O2Ky9hPDtlOTxvKyg8NTBHOCo4KiMxVjpETztFRzlJUjVBR0BaKz6AYmGeSGGcHCA9
HB4uHx8yHR4yHx82IyM+HyE6KClOIidQKzd7LTyIN0eaOU2oQFSsQFaxOk+pNUmfNkqjO0ul
QlSmR1ajLDl3Hh8xHBwxISE6HSA6JitHLjJVICJAJSZAVmypSWKgT2idTmWYTWScT2WbTmKS
TV2PSFqMR1aIRliHgrPzirn2irfxjLfyVXXGV3nMUXfLQletQUB0JylWS0iDMSxONSw9JiAt
OCs8SztFQjBAUz5MQzlKSTlKPlWjcX2zOUp6U1mGISExHx4wHyA4Hh80JilDHyA3JypPJzBo
LUGQM0WaOEynPlOvQlu3OVKxM0utN024Tlq/cnrQcXzPSVWrQFWdHyVJHR4xHB0wHyE8HCE6
JSZLGx06JyhDR16aQ2GlTmmkTWScUGaeTGKYUWSXSluRTV6SSluOSV6TcqbthbTzgrHvh7Lv
UnPFUHPGUnfMPlyxJy9oRkJ4ODpyNy5OOi0+JyExLCczQDE+RDFBQzFDSzxLTjlHREB0hIGs
d32sKCpaNTZgHhwxIiRHISE4JidCHh41LjBbKjZ8N0mhNkqkPVKvQVezQFayLj+NLDiALz2O
LjuGJTaAP0upNEOXN0iQMkGGHh8yICA0HR02HSA8ISRGHiI7NTpZOkyAQ2CoTmilTmWfTGGZ
T2KXTF6SSl2TSV+VS12TRViMYJThgbLwfq3sfq/uTm7DVHTKVXnNP12wJy1iJypcQ0F/PjVY
Tz1NKCQyKyMzLCQzNys7Pi8+XUJYRkdqHR0uODhVWVyDTmKcR1GHHitRJiRBHyA2JydCGxw1
LTJlLT2MOEukPE6rPlGyPlS0Q1i1Rly5SV22RluxT2O6WW/Ic4zceILPU2W1RU6aIChMGxwx
HB0yJCZHIyZGISM+OT5lPkRzRWClTWajUGafT2OcTmCYTGCYUGKYSl6WR1uTRFaMVonVc6fu
fa3udqnqTm3DUXLIU3bPRF+zJy1gJitfNjdyOjNeLig6PzJCJB4tQS9BUTlIaE1XWEBPLic5
IixTKSc2NzZMWl6COURyTWGXHhwuHiAzIB83Hxw0KDBsMECSNEiiOUuoQlW1QVe2SV+9UGXA
VGrDUGW7SmC0Vmq8d4TQiZnaWW68RFSkMTqAHh80HR4xJidLHB4+IyQ/Uld+VVuFP1iaTWSg
TGWhTmOeTmObSmCYTmGZS1+XTF6TRFeHVoTOYZnleq3ve6rtTm7DUnTITnPIPFiuLCpkKy1l
Nzt2PTVjNSo8RTlTSjlJNSo5RS45WUNUUUNHHx4uHjFIKyYzMDBWLChEKTFPSF2ZbXSVIR81
Hx8yIx02MjpyKzyLOkymPU+tQ1e2SF26TmPCUGXCUmi/S2G2OU+kOk6nOEqYZXK8W3G9RFem
OEeVISNGHx8yISNAHR8+ISM4U1qJQkZrO0uCRmCgT2ikTWOfUWWcSl6WSV2TS16WSluSQ1eI
T3a+X5fmZ5/oeqvtUHDGUnXJUnbON1OoLC9ePjx3NjduNS5hNyk8SzdLMyQzNy9AVz1JWkRP
JiEzHx4uIyMzHiIvMD+GJylHIyMxVGWNQVOEPklzHh8wJCE5IildLD2JN0mkQlSxRFm4SF69
TWO/UGfBUmm/PVOpPFGoNkqgNEimUmnCeI3TRlurPU6eKTV0Hh4zIiQ8ICFBIyQ3V2OYNTdX
MzlbQFmaUWilT2aiUGWdTGGaSl6VSF6XTV+VSluTSmahYJfkXpXjbaHnUW/CTnHFTnHJN1Kr
JipdODtvU015SkRwPzVLRDJRNSo3TjpGYEJKSTpWIBwtIB8rIyEvLzhlLzZqSkmDKSVIGh8o
MUiDh5O/IiVHISM2IydWMD+OOEukP1KvTWPBUWfGUmnDV27HVWvDP1awNkmgNkuiOU+nY3LH
sLnqTGKxQ1WlL0GNHiFBICA7JCdIHiA5Y3GkIyQ/ISI8QFeWT2mnTWemSmOgSmGcT2SfSl+Y
SFqQRlmPSFuPXo3VW5XkXpfjUHHFU3XITHHHOFOuJShaMDRlNDV4U0+FSDxLQzpUPTFEKx8s
SzdDNCczIx0vHh8rIB0rLCs/JyQ7OztoPUuHHR0sKjViZnKpcXuiICAwJCpVKz2OPU+pRVi0
S1++VGrIYXfQYXjQXHPKSWC7QFSsOU2mPVOugI3WipPOUGW0SFirNkeUJStcHR03JylJLS5J
VmWbHiA1IiM3QFCISWGbTmqrUWilTWSgTWKeTF+YTF6WS12URVaLUm6rXpbjWpPkT3HFU3bL
UHTJOlWrJy5gJy5mND+CRUB/RDhSLiM2KyU2PS9BPik7QDJFHh0qHh8sIyEvHRwsJiQxOj1q
ISY6Hx0uIx4vPUuHaYK/SEp7ISRRMEGVOU2nSFu4U2fCXHLMaXvRZXzOX3XJTWS7Sl62PlOt
RViwmZ3ggpLRV2q5SFisOEmaJzBwIiI/IiFBQEFiTFKDIiE1IiE2NT5kR1qSUWqnTmaiTGOf
SmGcS2CbTWGZS1+VSVuSRFWKV4HIXJblUXPIU3nPVHjPOlaqKC1hJjBpKjZ6VFCFX1V2SDln
MSU6JSExSDpLRjFELCE1JCIuOys6V0FNPjRJMTBLJSM3Hx4vHx4wMTReU2OghJPDKzZnMUGN
P1KnSV21XXDHZXjOZ3nLZ3vMZXjIU2e7RlmvPE+rRVeyp6rlgI/SVmm2QFKnOEaXKDJzHx8/
IyFCPD9lPUFqIiE0IiE1LjBKQ1aNUmmnU2qmTWSjS2CbTmSgTWCaSVySTF2VR1iMSFyQVoTK
TXHGVnrRUnjPPVmsKjBjKDJqLTyANkGMUlaJQkJoNStMJyM5JyIwVT5KVj5LKyc0NSs4Pi8+
YUdWMCtBHx0tHx8vIB8xHh4uQEqCa4LAe43FKDmCPEueSl6yU2S5UmW5UWC2Wmq1W27AW2u5
RFSmPU6oT2K8qq7pdILATF6rO0iYLDZ4Ji9nHx8+JSVCPkRpOTxgISA0IiE1Hx4zRlaJTmSf
T2elTWWjTmWiTGOfTWKdTWScTWKXR1uPR1eKTWKUUHTKU3nPT3LHN1GlJixcKjRtKTZ0PEyJ
KipRNDVRV1eYaHeoNS5ZNTFBNCY5MCc4LyMzUztJQzJCJSAzICAvIR4vICAxHx8wJzBXWmeb
cX+3eIG3NkWTQlCgR1epNUKMPkmNWF6eX2GnUVyyOUqaOk2kUGW7rbLtSVSQR1CUU1COJC1g
HiNKHh46JSQ/SUptNzlfIiA2Hx8wJSU5PENuRliPVGupUmmnTmWhUGeiT2SgS2KeUGWfTF+a
S12SR1iMTnLIUnfKS23BNU6fJSdTJSxhJzJxKjZ2LjVvJi9kX2+pQ2GxQlekJy9XJCMtMCY5
Lik7QDE+Nig8IB4rHBwqOzRDHxwuICMyIyM0MDphZHCegY2+cYC3NUWNLDNfKCtUKypLRkNd
l5nOOUOMMD+KOUugUWi9mKXhM0KCKCpGNzZYQUqIHR83HB4zJyVAT1FyJiVHJiY8Hx4xIiI2
LC1KRliST2ikTmelUGWgTGShTWOjTWSjTmOgTWSgS2CVSVuQT3HEU3fMTG7CNU6fKCpUKC1f
KTJqKzZ1Kjd2JzRyVl6RQFOdRV6uNUN8MTJsKCQwPCtHOCo+Tz1SJCAvMio7HhwvMSo3RTdH
HRwwKSc+TF6ZXHi4iZK+anOxIzBoIiNEHR83JCQ6NzdVJixUOU2hPFCqU2zEgJDabYDEIidS
ISA5KitIHiA5HyA6IiI3Q0ZxJydHKSc/IB8zICA0IyA1Q1SLSl6WT2enTmalS2SjTGWlSWOi
S2SfSmObTGObS2CWTnDEUnTIUHXKNk2dKSpUJSpYKTNqKDNxJzNyKjZ2KjiANUONP1ahOUeK
LzJTMD9+ICY/PjFDMyY3RDJEICAtISAxLyU5Hx8uMSg7KiU9KjBRZ3WxdIW9cYG0aHqxOkyM
NkWPOkmOQUyYKzV7LTd5S2K5WnPLdYvaeIrXW3HBMT55JSxeKjZyJSdLJCE5Sk16JiZBJyY7
IB8zIiE0IiE1PkZxR1WITmmoTmenUGimTWWjS2OeUGijTWWfTWWaTmKYUXDCUHPGTm/EM0qX
KClSJipbKzZsKTRzKjRyKjd4Lj2BNkaNMUCPMkaGKjJhdXurNEOCLytLLSY6SDVEIiAvJSI4
JSQ1Hh8vLio5LSY8KCU4NkR+UGmmbH+3Z3WshpK5T1+lOEWCPEWBRlabZnjLZn7UZ3/UeI/b
d43ZhJjiepLhVmiyLDJqHh02JyY/VFmIHB00IiE2IiI3Hx8xIiA0MDBMQVCFT2ahT2ioUWql
T2afT2efUmqkUWWdT2WcUWaZUXLGUXPDUHHENUqWKChUJipZKzRsKjVxLDZ3LDh6Lz2BNEOR
MEOWMUaLMEN5MD+JSkiAQ0J6JiQ2Nis7JCAwKSQ3LCU6IB4vIhwtLSQ5JiExIyQtNj95XW6s
X3azgY29bnynZXSyWmy7XXDBZnnMaYDUdYvbh5vkh5njjJzil6fpj53hNEaQIiU9JCQ8TlWD
HyA3IiI3JiQ4ISE0IB4yJSI1Q1CESFmRVG6rUmukU2ujUmylVGujUmecTmGTTFuJTnDET3HE
T2/EOE2aKCZTJitcJzBqKDNvJzJxKTN1NEGKMj2FNEOQMkCDUXK4M0KGP0mMRE+XMjZ4KiM8
IB8tLSU6JyM1IB8wJCM2JCAxRTdMQThMJzFOUGGeZoC8cYO3ZGqThI+9aHeuWGy1X3XGaH/S
fpLcip3jjaDklaPmmKbnfI7WLzh3ISA0KihEU1WFGhwvHyA1IyI2ICA0Hx8zIiE0OD9kSFqM
UWupU2ulU2idUGWbTmGWTl+PU2OWUF+STnDEUXTIS2zAN0yZJiNQKSxdLjdwJDFtLTZyKjV1
MTl9Lz2GLzyDSV+cYYjXLzt7NkOFPkuSLjx9Ky1ZKCpJKSM5ISAwISAxLig9KyM4HR8vPTBB
ISAzKDFUS1mYYHe2b2yLc3+vbnyxaHWlX3CyZHfId4vWgpXbjJ7jlKXnl6jrX3TCJCpaIyM4
SEZgREZsHh4xHh4yJiU4JSQ4Hh0vKig8LCk/RVSHTmGWUGOcUGCWUWObUWWdU2iiVWqlV2ul
UHHGUnTISWrANEmXJidKIyZWKDBnKzRvJzJsJjFvLzt9MT6FMD19XXCtb53pNU6SPEiEOUSF
PkaNOkydMTVrLy1RJiQ3Kyc6MSpBKSU2Myg7PjFEICAxISA1LCtESEl2fICVbX+3doS0Y3Kn
dICxYnKqbH29gZTajp3hipfbhJXaRFimJCpNLy5MR0ZnPEV2ICAvHBsuIyI2JCI2Hh0wISAx
JSIzQElyRlJ8UWWbV22pVm+rV22nUmekVW2qUmekT3HDUXPGSGm/N02ZJiZLKS5ZKjJoKTRu
JTFuJDFuKzl8Kzh5LTp2dozFWoXNPVCXOkeJQEiGPEaDVU18S06LPD1yNjBYNi1JKyQ7IiAw
NCpDIh40JiI6Jh81Mig8U0BUW1V0bXigdoe8c4O1bnyxcH+zcH+veYe3hJDQhJPZfI3PNUKL
LS1cPDtZT09zQFSYHh0xHB0tIyM2ISE1Hx8yIyMzJCI2MC9LPkhzUGKZWHCrWG+pVm6pVGqn
WW+oVGqlT3HDTHHERWe8MEWQJSZLKCtcJzBlKjRtKjRuKTRwKjl7KTd2P1mNaazyVXe+SmCi
O0uMPkuMOUSFRE2KQ0uPPkabMDJgLSlJKyE3KCM3KCU7HR0tMyxKKyk+QTFKLyU8QjdKVlV/
d4i9fpLFdYW1dYOzan2wc4Cxfou5c4G2XGyrKCxVKipJSEZkSk14XHa+UG6uISlMHx8yIyI2
Hx4yHyAxIyE0JyI2P0h0S1iOWHCrVWymVGqnVW2pVGmjUGajTXDDT3DCR2m9L0SNJyhMJSpa
KjNqJzJrKzVwLDdyJzV0KjVsZ3y6SX3lSGq7O1WmMzx5QlKXPU2UPkqJQ06WMjxzQk+SOkaO
PT9yMCg/MytDIyI1OzFDNClJNi1DPDBHKSU6SkFOOk6Lc4G1c4a8doW3eIe2dYa2eYi2e4y6
iZa7XG6gPT5lPTpYWWSbTW6+VXO+Y3m/KjRWISA0ISE1ICAzISAyJCI0OTxeTVyXU2elV26p
V2unVGqmV2ulUmajTnHHTm/ESWq+L0SOKitRJy1cJy9iKjNvKzRuLDd0KjRyKjRshrPtYnzN
PlKsNEqMMj+CMjx4PE+WOUaIPEiMQU+VPk6PQlOcQESCQ0F8QDZXPTNWKiU8STdMRjZPT0Fe
MixBHiAyIx4wUlqQa3SrboXBh5bFhJLBe4y8gpHAi5fCj5jGjZjFbnSiMzdjLzVhO0l/T2Sh
Zn/CLTZXHx4yIiEzIB4xJiM4JiM3TFqXTV2bVWuoU2ilU2mlWWylUWWgT3LFT3HESWm/MkOM
JidNJyxaKzJlLjdxKTRtKTVxKjZ1MT5yUZPvZpHWOFWhO1ipOUWIOEF+OkV/O06TQVCXPEeK
QlGSRVScS1WdU1SaRkZ5QThlPTBSWVl4Yk1mSTtYQDRILCY+ISA9KyY+U1qDXm+tg5XHhpfJ
gI+/g5O/i5nEj5zHk5/MkpzJjZnQWWWXMDp0KTFYOkZ8Zn25P0RiISAzHx4xHx8zICAyO0Fq
SlqbVGmmUWipUWenUWWkT2KfTW/DTm/DQ2a8LkCIJylQJCpaKDFmKjNsLThtKTVwKTVyUVuO
cI7XPWzRQF+sNUykN0OFQE+aRk6NNkKFQ1ekRVSgRFGVRVaZTFqfSVeUUVSNVFCHSE6HY2uY
VleCTVaFWmepXWajZG2tS1CDUlWAYGukb4G9d4u9iZXGhpTCipbCjprFkJ7KlKDNlqLOlZ/I
mKbPVGWlND5vPUVweIvBHB8xHx4yIiEyJSQ5Kic/SluZT2CfUminUWepTGKgUGakS23BTGy/
RGa6MUGLJyhQJy5fLDNqLDZwKDNqKDVxJjRwPD95WJPYP1e0PFm6MkaUKzl0PEmMQE+bQE6K
O0eKQVakQFKfSFObTV+ZUWWrUGSrYGq0am+yamukYHO8VmapX3SuXm+0XGyyXm6zZ3CpYXCu
XGmhaoCxdYbEhZPIfZPJhJG/jJjDkZzFk5/LlZ/In6fOiZTBXm6cT2GgaHOhQlByIR8yICAy
ICAzJCE1QENtS1ycVGilUmioU2qqUWWmUHDETnDCRWS4MUKHJyhQKC5fJzJrKDJuKDNsJjJv
Kzh5KDBqUpXuRGexN0+VMj+CMDl1NkOHNUGERFalVGCnO0yTUGGsS1mgUF+jWmasVGGjYnfB
YW6wZHSyYHK4ZG+yXHGwYHazaHizZ3i3doO+a3uybXywdH60cXuteZHDiY6/lKLMiZjHiJbD
jZrEk57Ik5zIoaTNany1X2ylU2GWXG2eIB8xISAzHx8wISA0LCpETFuXUV+aUmmnUWenUmeo
TG7CT3LHRme6L0CFJidPJSteKjJqKjVvJzFqKDRzJzR5KjVwUIjkN1esLj+EMDuAND19OEaE
O0aQPEmIRVunQVOgQlaVU2WvTF6dVWOqY3S5ZHW0aHq6XHO6bX+9anq9XXG2an+/aX3Fc4O/
doW6gou9cH61aXu3c36ucXqncoCwe42/hJHDk6TPkqLQj5rGkJzJlp/KkaHNY3WtiZfCmqLN
IyQ7ISEzICAyIB8yJSI2ODldTV6fTmKfUmmsUWepS23BUHHER2e5MECFJidPJitdKzRqLTdx
KTNtKTVyKjd5LTp3aJbiN0+iLDh2Kzd5KzdxMj18NkKJOEWLPk2PSF2pR1mhSVyeUmmyXXKx
Wm+3b3a0aHq2ZnK4ZHe/eITJYHy9eYjAbYTDaX65g4zCfYzAgou5hY3Ag5C+h466gomxlJTA
cYK+g5jOk5jOkqPQkaDJk5/Im6TOkZ7Jm6PKr6/VWWOIIyM1HR4wICAyIyE1KCc+R1CGTF2c
UmmnT2WoSWm7Tm7AQ2G1LTuAKChPKS9dJjBoMDt2LDhzLjp5Kzl+Lj6CaHq7NkeRLDRsLjqA
NEB/OESKNUWDPUuWMkCIOUiKUWWvS12nU2uvZna5aX2/bn+5cYXIg5LMbnq0donGfIzFe43H
jJbJpKXNdIe5eI27jZnJmKTRlJ/Ll5vEoqLHdoW5QWS8UmaxcoS9hpvJl6TNmqnSmKPPmKXQ
n6nPqK7TaGqFIiI0JCM1ISAzIB8zJSM3LCpFTF2cSVuXUGeoS2u8SGi6P1yuLz1/JiZIJChU
KTFmKzRuKjRxLjl2Kzh7MT+CT3K2OlWgKjVxMDt8KzdvN0WSNkaPMkGBOEmUNkiNOUSGY3Wv
XW+zWnG4a3zBcoLGeY3IZnu3kY/Ae4bAi5nMkJnQlaLVjJnNmKDQgZXCk6PPmKPLoafPqavQ
mJu9N1ShRmW6P2CyM0mRUE5yeYKojZi+o7Hal6bRkZzEqq/Ta3KhIB8xISE0IiEzHx8yIyI1
IyI0PUJoTV6bTWGdSGm8TGq6P1ywLDp8KChIJilVKTFnKTBmKTJnKDNtKzZ2Ljp9R1SOPl2t
LjdyLzl5NkOBNUORPUqIOEyXN0OJPlCWQlGVOUh8YW+vYHm9bHq7dYG4Znq6fZXLbYe/nJzK
hZHAkprJlaPTlqHTj6LQl6XQoKzToKrQqK7TsrPQLDdsP1mnRWW4RGO0OFGcJCtWIyI1bnaN
jJq+srrWqrXbo63Tb3GUIB8xIB8yIB8yIB8yHx8vJiQ2JCM2SFWJSFiRSmm8Smi7QFywLjt9
JSZJKy9eKjNnLjVsKTJsLDdzMDp8Mz+CMTp+T264Ljl3LTh4MT1/NUODO0eLOUqIQFGTSFyg
PFCTPEuOO0yTVl+UdIXGb3+5cYrGZH+9e4vIk5rIi5nLoqbXnKnWpavRm6bQpq/aoavToK7U
v77gaG+TLj55QFurRWK2Q1+wOU+bJCpXIyI3IiE0S0xkmajSoKvPubbOSk5zISEyIB4xISAz
Hh0vISAyIiEyIiE0KilBTFqVS2m8S2m6QFmqKjd8KSlNJypdKTFmKjNrKDJrKzZwMD19Lj2C
M0GKN0+UKzh6Ljt4ND2BN0CGNEJ+OEaLQ0+NSViqQVCVRFOaPk+dQlGUUVyccH6ydIfEgJbO
cIvJiJXLoabTm6zcoazUpq/Wn6nUoazUoqjQsLfblpy9MDNfLz5/P1mpRWCwQl2tO1GdJixZ
IyE2ISM7KzduO1KjQFWgPlehOE+cIB80Hh4vIB8zISA0Hh4uIR8zHx4xJCM0MjNSTGzASme5
QF2vLTl9JidLJi5ZKTFlKDFrKzRtLDVxMz58M0KGMUCIMkKMM0SMLj19NUKBMkGFOUSFN0SF
PEuFPU+WQ1SWRlOXQ1mjRleUSV+nVm2qcX2xfpHHkaLRmKTTiZbHmp/Noa3dmaHLo67XpbLd
pLHVl6DANDljJi5eM0WKQl2uRGGzRmCwOlGcJStYISA1Ji5bO0+bQV2xO1SkOlWnQFqpLz96
HR0vHh4vISA0ISAyHyEwHh0wIiM1Kyk6RGi+SGi7PluuLTt9JSdMKS5hLDNnLDVvKTVtLTdy
NEB+NUSJNEOPNUaOOUmRM0KGMj97NUKDN0OANkF5N0OAOEiIQ1KNQFSbQlWdSVmeTl2dVm2r
c4K/eICyl5vDmKTTr7Dcpq3ZoKvSo67XoarUt7rakZnAJCpVJS5eKzRqNkeNQFuvRmK2QFyu
NUybJSpVIiREM0KEQ1ytP1qsPlelPVanPVeqQlquIylOHR0sIB8yIiI0Hx8vISEzICEvIiE0
Sm7BS2i9PVuwLDp/JidNLDJjKjFlKzVsKjRuKTRsMT9/MUCFMUKKNkWMNEePMkaRLDp+MD16
OUaGP0qRPkyMOEaKQE6URFaUQUiDSlqiTV6hXHOzcYW8d4u+bIS/jZXEoanTqbHcpq7YoK3X
oK/boKjHKDFeLThvMDlwKjRtPE2TQF6yRGK3QF2xO1GeJClYLDRtPVOhQlywPlmsOlOkQFur
OVWlQlqsN1ChIR84Hh4tHyAxHx4yHh0tIB8zHB0rTHDDS2y9PF2vMD2CJihNKS9fLDJlKzVn
JTBnKzVyLzt8LDyBMkGGM0SLNUeQMkWSPlSeNEOBMz1/NkSBOkeLOUePOkmKQ1CVR1KWTl6i
TlmSXXGndoO8gpDCeI7Fh5fGgJPJgYu6kKPLqKvJsa/FSFN/LTl2Ljx4MDx6Lzl1NUiQQl6z
RGO5Ql+yNkuXLTt8Ok2YQVytP1yvPluvQVusP1mpO1apPlurPlusMUSHHR0tHx8tIiE0Hx4y
ICEvIB8xS23ARWe5P16xLDuAJShLIilXKjFmLTVsJjFsLTh0Lzt8NEGIN0WJNESLNUiTN0eT
NEiNOlGPPUiLPEWBPEaERFCOQEqMSVaUS1aTTGCUXmyzbnmuZ4K5dou5forAgJLGgZbMfpPB
h5nFWGOTLz17LDt6MkCBLTx6Ljp5MDt4M0aOQFutRmG1P1mnPVSfOlCbQVuqPlmrP1quPVuu
PlenO1akPliqPFmoQF2sQmCvN02QHyEwHR0vISA0HR4vHh8uS2/CS2vAPV6yMEGFJihQJy5f
KzNqKjVwKTVzMTx6MkCHMUCJNESPNUaPNEeSO0uXOUiZPEeYVFyWRFCMPkiHQE2DR1GKQ1eL
UmCgWGKsWWebZnusboO3dYW8gIzBe47IepHDcYGwRlGJMUCGNESLNEWNOEeLMT+DMD16MDx3
OUySQVqqQFurOlalQVqnQFqqQVytPFemP1uvQFurPFikPlupO1akPVqpPFmmVHzLh7bzPVGP
ICAyHRwtICAzICAyTGy+R2i8QGC2LDuEKSxaLDVtLDdzKjVxLTh2NECDNEOLNUaQNkmTOUyY
NUaSOEuXO02cO0+cO0ucOkubQVCbSVeZVl2WSlmUV2GgaHKrbG6ocX2tb32tZnqwdYO2fJG+
U2OWMEOML0GLM0aRNEaRN0eSNkeKM0OFLj9/MD18NkqUPlipPFalP1mnQlypQ1+wQ1+vQFus
QF2wQFysPVmnOlSkOFSkQFuoQGCve63xkb73VX7DIyM8HR8vIiIzISE0SWi8R2W5PVyzMT+D
JilYKDBqKzRyLTZyLjdzLDl7MD+HOEiROUmVPE6ZN0eVO0yZOEqeOEqcOEqbO06eOEqdNUqf
PE2aQFOXSFSVUmCbanOfdXuzX3atW2GeRFSONEOSMUaVM0qYN0uYOEmWOkyYOEuYN0mSM0WJ
M0KELzx7NkiQQFmmP1mnRV+tQmCwQ2O0Q2GxRWS3QWG0P16wPlqrOlenPlupPFupY5Lfibz4
cqLjM0R7LTlwICE5Hx4xHh4wRmW6SGe6Pl2zLj2CJyxcKjJsKjRwLTh0LDRvLjh3NkOJMkCJ
N0aPMUWQNUiUO0yYOEuZNkmbO06gOkyeOUueMkebNEudN0ufNkuZMEWTMkeWMUSQNEaOMkOT
NkiXN0uaNkybNEqbN0yeNUqXMkeUOEyYN0mVMkSNMECELjt8M0aQPlaqQ1+vRmS0RWO2QmK2
RGS3QmGzQ2O4QmK2P1yvPlusPlysT3bIhrf3iLn3QF2bKzhzMkF9M0B8ISM8Hh8vSmu+R2e6
RWS6Lz+HKi9iJzFsKjV1JzNzKjRyMTl7NkONOEmTMkSPOEycO0+fOlCfOk2bOE2fOk+hOEqc
NkmbNkqbOE2hNkueN0qaNEaWNkiWNkqWPE2ZN0mWO06bNUqbNEqcN02fOE6fNUmaNUiWOk6a
NkqUM0OKMj+CMT5/O02aQ1ywR2a7RGS6SGe7RGe7R2a5R2W6SGi+RWe7QmK0P16vQWCwcKXu
j7/8YY7VLTt0L0B/NUaFOEqKMkKBIiQ/S26/SGm7RGO4Lj2CJy1eKzVyMTp5LTh1Ljh0MT18
Lj2GN0iPNUmUOk+dOlGgO0+cPFCdOlCeOU+fPVGhPFGgO1CiN0yfOE2iNkudNUuaOU2bOk+b
OUyYOU+eOVCfOlGhN0+eOU+fNk2bOE2aOE6aN0yYNkuTOEmNMj9/MD1/Ok6ZR2O2SGjASm3C
S2vARmu/Q2S6SGi9R2e5RWS3Q2SyQmGvV4TTgrf5eazwNkyGMEJ+NUeEN0mIN0uJNkqLN0iM
TXHESm/DQGG2LT2AKC5eLTZvMDp3MDt2Lzt1MUB+M0SKO0qQOE6WPFKdOlOePFOfPVSiO1Og
PFSkPlOkO1KhO1KfN02dOE6fO1GhO1KhOlGfOVKgPFOePFWgOlOgOlKdOlGeOlGeOVKdOlGb
OVGaOFCZOE+UNEmMNkWDLjx6P1eiR2i5THC/SGzASW7DR2vAR2m9QmW5SGe3RmSyRWSzS3LA
dqvwgrf4RWqpLkB2N0eANkmFOUyGOk6GN0yHOE+S'
			base64Decoded asByteArray readStream! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 1/14/2015 15:11'!
testComposition
	"
	AffineTransformationTest new testComposition
	"
	| composition inner outer |

	outer _ AffineTransformation withTranslation: 3@5.
	inner _ AffineTransformation withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ AffineTransformation withRadians: 0.3.
	inner _ AffineTransformation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 6/8/2020 11:08:17'!
testDisplayBounds
	"
	AffineTransformationTest new testDisplayBounds
	"
	self assert:
		((AffineTransformation withRadians: 0.3) displayBoundsOfTransformOf: (10@10 extent: 20@30))
			= (-3@12 corner: 26@48).

	self assert: (AffineTransformation new displayBoundsOfTransformOf: (-2@ 2 extent: 10@10)) = (-2@2 corner: 8@12).
	self assert: (AffineTransformation new displayBoundsOfTransformOf: (-12@ 12 extent: 10@10)) = (-12@12 corner: -2@22).
	self assert: ((AffineTransformation withTranslation: 2)  displayBoundsOfTransformOf: (-4@ 2  extent: 10@10)) = (-2@4 corner: 8@14).
	self assert: ((AffineTransformation withTranslation: -4)  displayBoundsOfTransformOf: (2@ 2  extent: 10@10)) = (-2@-2 corner: 8@8).
	self assert: ((AffineTransformation withTranslation: 2)  displayBoundsOfTransformOf: (-14@ 2  extent: 10@10)) = (-12@4 corner: -2@14).
	self assert: ((AffineTransformation withTranslation: 4)  displayBoundsOfTransformOf: (-12@ 2  extent: 10@10)) = (-8@6 corner: 2@16).
	self assert: ((AffineTransformation withTranslation: -4)  displayBoundsOfTransformOf: (12@ 2  extent: 10@10)) = (8@-2 corner: 18@8).
	self assert: ((AffineTransformation withTranslation: -2)  displayBoundsOfTransformOf: (4@ 2  extent: 10@10)) = (2@0 corner: 12@10).
	self assert: ((AffineTransformation withTranslation: 4)  displayBoundsOfTransformOf: (-2@ 2  extent: 10@10)) = (2@6 corner: 12@16).! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 1/21/2015 13:05'!
testFloatInverseTransform
	"
	AffineTransformationTest new testFloatInverseTransform
	"
	self assert: (AffineTransformation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((AffineTransformation withTranslation: 4) inverseTransform: (-2@ 2)) = (-6@-2).
	self assert: ((AffineTransformation withTranslation: -2) inverseTransform: (4@ 2)) = (6@4).
	self assert: ((AffineTransformation withTranslation: -4) inverseTransform: (2@ 2)) = (6@6)! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 1/21/2015 13:06'!
testFloatTransform
	"
	AffineTransformationTest new testFloatTransform
	"
	self assert: (AffineTransformation new transform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) transform: (-4@ 2)) = (-2@4).
	self assert: ((AffineTransformation withTranslation: 4) transform: (-2@ 2)) = (2@6).
	self assert: ((AffineTransformation withTranslation: -2) transform: (4@ 2)) = (2@0).
	self assert: ((AffineTransformation withTranslation: -4) transform: (2@ 2)) = (-2@-2)! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 1/14/2015 15:12'!
testInverseTransformation
	"
	AffineTransformationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ AffineTransformation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7).


	forward _ AffineTransformation withRadians: 0.25.
	inverse _ forward inverseTransformation.

	self assert: inverse radians = forward radians negated.
	self assert: ((inverse externalizePosition: 3@4) - (forward internalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeDelta: 3@4) - (forward internalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeScalar: 7) - (forward internalizeScalar: 7)) abs < 0.0001.
	self assert: ((inverse internalizePosition: 3@4) - (forward externalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeDelta: 3@4) - (forward externalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeScalar: 7) - (forward externalizeScalar: 7)) abs < 0.0001.! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/24/2020 16:52:18'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 _ LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 _ WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 _ WidgetMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 _ WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 _ WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 _ WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 _ LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 _ WidgetMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 _ WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 _ WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 _ LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 _ WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 _ WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 _ WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld: UISupervisor ui; morphExtent: 408@300.
	UISupervisor ui doOneCycleNow.

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth = 10.
	self assert: r1c2 morphWidth rounded = 200.
	self assert: r1c3 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c4 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c5 morphWidth = 20.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth rounded = 216.
	self assert: r2c2 morphWidth rounded = 108.
	self assert: r2c3 morphWidth rounded = 54.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: r3c1 morphWidth = 20.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c3 morphWidth = 30.
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphHeight = 40.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphHeight = 50.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/24/2020 16:52:35'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 _ WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rowBottom);
		addMorph: (c2 _ WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 _ WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld: UISupervisor ui; morphExtent: 400@300.
	UISupervisor ui doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 261.
	self assert: c1 displayBounds bottom = (row displayBounds bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = 200.8.
	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 displayBounds top - row displayBounds top) - (row displayBounds bottom - c3 displayBounds bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight rounded = 176.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/24/2020 16:52:42'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ WidgetMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ WidgetMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ WidgetMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (c2 _ WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 _ WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld: UISupervisor ui; morphExtent: 400@300.
	UISupervisor ui doOneCycleNow.

	self assert: row displayBounds left = (pane displayBounds left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow displayBounds left = (row displayBounds left + 5).
	self assert: (innerRow displayBounds top - row displayBounds top) = (row displayBounds bottom - innerRow displayBounds bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = 170.
	self assert: innerRow morphHeight = 30.

	self assert: i1 displayBounds left = (innerRow displayBounds left + 5).
	self assert: (i1 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i1 displayBounds bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 displayBounds left = (innerRow displayBounds left + 20).
	self assert: (i2 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i2 displayBounds bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 displayBounds left = (innerRow displayBounds left + 35).
	self assert: (i3 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i3 displayBounds bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 170.
	self assert: c2 morphHeight = 40.
	self assert: (c3 displayBounds top - row displayBounds top) = (row displayBounds bottom - c3 displayBounds bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !

!MorphicLocationTest methodsFor: 'testing' stamp: 'jmv 1/14/2015 14:32'!
testComposition
	"
	MorphicLocationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ AffineTransformation withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ AffineTransformation withRadians: 0.3.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:46'!
testComposition
	"
	MorphicTranslationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ MorphicTranslation withTranslation: -1@2.
	composition _ outer composedWith: inner.
	self assert: composition translation = (outer translation + inner translation).
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MorphicTranslation withTranslation: -1@2.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:19'!
testDisplayBounds
	"
	MorphicTranslationTest new testDisplayBounds
	"

	self assert: (MorphicTranslation new displayBoundsOfTransformOf: (-2@ 2 extent: 10@10)) = (-2@2 corner: 8@12).
	self assert: (MorphicTranslation new displayBoundsOfTransformOf: (-12@ 12 extent: 10@10)) = (-12@12 corner: -2@22).
	self assert: ((MorphicTranslation withTranslation: 2)  displayBoundsOfTransformOf: (-4@ 2  extent: 10@10)) = (-2@4 corner: 8@14).
	self assert: ((MorphicTranslation withTranslation: -4)  displayBoundsOfTransformOf: (2@ 2  extent: 10@10)) = (-2@-2 corner: 8@8).
	self assert: ((MorphicTranslation withTranslation: 2)  displayBoundsOfTransformOf: (-14@ 2  extent: 10@10)) = (-12@4 corner: -2@14).
	self assert: ((MorphicTranslation withTranslation: 4)  displayBoundsOfTransformOf: (-12@ 2  extent: 10@10)) = (-8@6 corner: 2@16).
	self assert: ((MorphicTranslation withTranslation: -4)  displayBoundsOfTransformOf: (12@ 2  extent: 10@10)) = (8@-2 corner: 18@8).
	self assert: ((MorphicTranslation withTranslation: -2)  displayBoundsOfTransformOf: (4@ 2  extent: 10@10)) = (2@0 corner: 12@10).
	self assert: ((MorphicTranslation withTranslation: 4)  displayBoundsOfTransformOf: (-2@ 2  extent: 10@10)) = (2@6 corner: 12@16).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:21'!
testInverseTransform
	"
	MorphicTranslationTest new testInverseTransform
	"
	self assert: (MorphicTranslation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2)  inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((MorphicTranslation withTranslation: 4)  inverseTransform: (-2@ 2)) = (-6@-2).
	self assert: ((MorphicTranslation withTranslation: -2)  inverseTransform: (4@ 2)) = (6@4).
	self assert: ((MorphicTranslation withTranslation: -4)  inverseTransform: (2@ 2)) = (6@6)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:46'!
testInverseTransformation
	"
	MorphicTranslationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ MorphicTranslation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/21/2015 13:08'!
testTransform
	"
	MorphicTranslationTest new testTransform
	"
	self assert: (MorphicTranslation new transform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2) transform: (-4@ 2)) = (-2@4).
	self assert: ((MorphicTranslation withTranslation: 4) transform: (-2@ 2)) = (2@6).
	self assert: ((MorphicTranslation withTranslation: -2) transform: (4@ 2)) = (2@0).
	self assert: ((MorphicTranslation withTranslation: -4) transform: (2@ 2)) = (-2@-2)! !

!WorldTest methodsFor: 'tests' stamp: 'jmv 10/24/2020 15:58:12'!
testDoOneCycleWorksWithDeferredQueue
        "Ensure that nested doOneCycles don't break deferred UI messages"
        | finished |
        [
                UISupervisor whenUIinSafeState:[ UISupervisor ui doOneCycleNow ].
                UISupervisor whenUIinSafeState: nil "whatever".
                UISupervisor ui doOneCycleNow.
                finished _ true.
        ] valueWithin: 1 seconds onTimeout: [finished _ false ].
        self assert: finished! !

!SmalltalkCompleterTest methodsFor: 'testing' stamp: 'jmv 7/14/2011 14:26'!
testMessages
	"
	SmalltalkCompleterTest new testMessages
	"
	| fromSmalltalk fromUCompleter |
	fromSmalltalk _ Smalltalk allImplementedMessages.
	fromUCompleter _ Symbol allInstances select: [ :s |
		SmalltalkCompleter isThereAnImplementorOf: s].
	self assert: fromSmalltalk = fromUCompleter asSet! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 20:56:42'!
test000_AutocompletesMessagesFor_GlobalVariables_WithSelectorsFromTheirClasses
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTest '
		areSelectorsOf: SmalltalkCompleterTest class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < SmalltalkCompleterTest '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest class.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:01:49'!
test001_AutocompletesMessagesFor_Self_WithSelectorsFromTheBrowsedClass
	
	self
		assertEntriesWhenBrowsing: 'm1 self '
		areSelectorsOf: SmalltalkCompleterTest.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < self '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:02:19'!
test002_AutocompletesMessagesFor_Super_WithSelectorsFromTheBrowsedClass
	
	self
		assertEntriesWhenBrowsing: 'm1 super '
		areSelectorsOf: TestCase.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < super '
		areUnaryAndBinarySelectorsOf: TestCase.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:02:43'!
test003_AutocompletesMessagesFor_Super_WithSelectorsForUnknownClassesWhenTheBrowsedClassDoesNotHaveASuperclass
	
	self browseClass: ProtoObject.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 super '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < super '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:11:29'!
test004_AutocompletesMessagesFor_True_WithSelectorsFromTrue
	
	self
		assertEntriesWhenBrowsing: 'm1 true '
		areSelectorsOf: True.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < true '
		areUnaryAndBinarySelectorsOf: True.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:12:17'!
test005_AutocompletesMessagesFor_False_WithSelectorsFromFalse
	
	self
		assertEntriesWhenBrowsing: 'm1 false '
		areSelectorsOf: False.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < false '
		areUnaryAndBinarySelectorsOf: False.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:12:12'!
test006_AutocompletesMessagesFor_Nil_WithSelectorsFromUndefinedObject
	
	self
		assertEntriesWhenBrowsing: 'm1 nil '
		areSelectorsOf: UndefinedObject.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < nil '
		areUnaryAndBinarySelectorsOf: UndefinedObject.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:36:47'!
test007_AutocompletesMessagesFor_Characters_WithSelectorsFromCharacter
	
	self
		assertEntriesWhenBrowsing: 'm1 $a '
		areSelectorsOf: Character.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < $a '
		areUnaryAndBinarySelectorsOf: Character.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:15:01'!
test008_AutocompletesMessagesFor_Numbers_WithSelectorsFromTheirClass
	
	self
		assertEntriesWhenBrowsing: 'm1 1 '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < 1 '
		areUnaryAndBinarySelectorsOf: SmallInteger.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:15:25'!
test009_AutocompletesMessagesFor_Strings_WithSelectorsFromString
	
	self
		assertEntriesWhenBrowsing: 'm1 ''a'' '
		areSelectorsOf: String.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ''a'' '
		areUnaryAndBinarySelectorsOf: String.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:37:45'!
test010_AutocompletesMessagesFor_Symbols_WithSelectorsFromSymbol
	
	self
		assertEntriesWhenBrowsing: 'm1 #a '
		areSelectorsOf: Symbol.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #a '
		areUnaryAndBinarySelectorsOf: Symbol.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:38:21'!
test011_AutocompletesMessagesFor_StringSymbol_WithSelectorsFromSymbol
	
	self
		assertEntriesWhenBrowsing: 'm1 #''a'' '
		areSelectorsOf: Symbol.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < #''a'' '
		areUnaryAndBinarySelectorsOf: Symbol.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:42:33'!
test012_AutocompletesMessagesFor_InstanceVariables
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 instanceVariable1 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + instanceVariable1 '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 16:42:09'!
test013_AutocompletesMessagesFor_MethodArguments_WithSelectorsForUnknownClasses
	
	"I reference to SmalltalkCompleterTest directly and not thru 'self class' becuase this test has subclasses
	the #m1: and #m2: are defined in SmalltalkCompleterTest - Hernan"
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m1:.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m2: ! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:22:39'!
test014_AutocompletesMessagesFor_TemporaryVariables_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:25:42'!
test015_AutocompletesMessagesFor_BlockArguments_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 22:04:54'!
test016_AutocompletesMessagesFor_BlockTemporaryVariables_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | a '.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'NPM 3/18/2020 18:34:08'!
test017_AutocompletesMessagesFor_NilWorkspaceVariables_WithSelectorsForUnknownClasses
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: 'x '
		binding: 'x'
		to: nil.
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: '1 + x '
		binding: 'x'
		to: nil.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 22:05:36'!
test017_AutocompletesMessagesFor_NonNilWorkspaceVariables_WithSelectorsFromTheirClasses
	
	self
		assertEntriesForWorkspaceWith: 'x '
		binding: 'x'
		to: 1
		areSelectorsOf: SmallInteger.
		
	self
		assertEntriesForWorkspaceWith: '1 < x '
		binding: 'x'
		to: 1
		areUnaryAndBinarySelectorsOf: SmallInteger.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:27:14'!
test018_AutocompletesMessagesFor_ThisContext_WithSelectorsFromMethodContext
	
	self
		assertEntriesWhenBrowsing: 'm1 thisContext '
		areSelectorsOf: MethodContext.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < thisContext '
		areUnaryAndBinarySelectorsOf: MethodContext.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:27:39'!
test019_AutocompletesMessagesFor_ClassVariables_WithSelectorsFromTheirClasses
	
	ClassVariableForTesting _ 1.
		
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTesting '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ClassVariableForTesting '
		areUnaryAndBinarySelectorsOf: SmallInteger.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:28:01'!
test020_AutocompletesMessagesFor_PoolConstants_WithSelectorsFromTheirClasses
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < X '
		areUnaryAndBinarySelectorsOf: SmallInteger.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:39:25'!
test021_AutocompletesMessagesFor_BlockEndings_WithSelectorsFromBlockClosure
	
	self
		assertEntriesWhenBrowsing: 'm1 [] '
		areSelectorsOf: BlockClosure.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < [] '
		areUnaryAndBinarySelectorsOf: BlockClosure.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 23:39:55'!
test022_AutocompletesMessagesFor_ArrayEnds_WithSelectorsFromArray
	
	self
		assertEntriesWhenBrowsing: 'm1 #() '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #() '
		areUnaryAndBinarySelectorsOf: Array.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:04:04'!
test023_AutocompletesMessagesFor_RightBraces_WithSelectorsFromArray
	
	self
		assertEntriesWhenBrowsing: 'm1 {} '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < {} '
		areUnaryAndBinarySelectorsOf: Array.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:43:31'!
test023_AutocompletingMessagesFor_UnaryMessages
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < self class '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:44:50'!
test024_AutocompletingMessagesFor_RightParenthesis
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 (1) '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < (1) '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 02:08:24'!
test025_AutocompletesMessagesFor_UnknownIdentifiers_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 Foo1234 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 foo1234 '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:45:10'!
test026_AutocompletingMessagesFor_Cascade
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class; '.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:13:19'!
test027_AutocompletesEntriesFor_GlobalVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTes'
		areIdentifiersBeginningWith: 'SmalltalkCompleterTest'.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:14:50'!
test028_AutocompletesEntriesFor_PoolConstants_WithIdentifiersBegginingWithThem
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X'
		areIdentifiersBeginningWith: 'X'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'NPM 3/18/2020 19:56:46'!
test029_AutocompletesEntriesFor_WorkspaceVariables_WithIdentifiersBegginingWithThem
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: 'x'
		binding: 'x'
		to: 1. 
		
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: 'x'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:15:11'!
test030_AutocompletesEntriesFor_ClassVariables_WithIdentifiersBegginingWithThem
	
	ClassVariableForTesting _ 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTestin'
		areIdentifiersBeginningWith: 'ClassVariableForTesting'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:15:28'!
test031_AutocompletesEntriesFor_InstanceVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 instanceVariable'
		areIdentifiersBeginningWith: 'instanceVariable1'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/2/2020 21:36:46'!
test032_AutocompletesEntriesFor_MethodArguments_WithIdentifiersBegginingWithThem
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: SmalltalkCompleterTest >> #m3:.
	
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (completer computeIdentifierEntriesBeginningWith: 'arg1').! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:15:54'!
test033_AutocompletesEntriesFor_TemporaryVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 |xx| x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:16:11'!
test034_AutocompletesEntriesFor_BlockArguments_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 [ :xx | x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:16:27'!
test035_AutocompletesEntriesFor_BlockTemporaryVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 [ | xx | x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:16:43'!
test036_AutocompletesEntriesFor_IncompleteIdentifiers_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 Obj'
		areIdentifiersBeginningWith: 'Obj'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:17:34'!
test037_AutocompletesEntriesFor_ReservedNames_WithIdentifiersBegginingWithThem

	self assertEntriesWhenBrowsing: 'm1 self' areIdentifiersBeginningWith: 'self'.
	self assertEntriesWhenBrowsing: 'm1 supe' areIdentifiersBeginningWith: 'super'.
	self assertEntriesWhenBrowsing: 'm1 tru' areIdentifiersBeginningWith: 'true'.
	self assertEntriesWhenBrowsing: 'm1 fals' areIdentifiersBeginningWith: 'false'.
	self assertEntriesWhenBrowsing: 'm1 ni' areIdentifiersBeginningWith: 'nil'.
	self assertEntriesWhenBrowsing: 'm1 thisContex' areIdentifiersBeginningWith: 'thisContext'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'NPM 3/18/2020 19:15:44'!
test038_DoesNotAutocompleteEntriesFor_UndefinedIdentifiers

	| completer |
	completer := self createCompleterForBrowsing: 'a'.
	
	completer computeEntries.
	
	self denyHasEntries: completer ! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:18:55'!
test039_AutocompletesEntriesFor_BinaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 ~='
		areSelectorsOf: SmallInteger
		beginningWith: '~='! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:19:17'!
test040_AutocompletesEntriesFor_IncompleteBinaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 ~'
		areSelectorsOf: SmallInteger
		beginningWith: '~'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:19:42'!
test041_AutocompletesEntriesFor_UnaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 not'
		areSelectorsOf: SmallInteger
		beginningWith: 'not'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:19:53'!
test042_AutocompletesEntriesFor_IncompleteUnaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 no'
		areSelectorsOf: SmallInteger
		beginningWith: 'no'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:20:03'!
test043_AutocompletesEntriesFor_KeywordMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 at:'
		areSelectorsOf: SmallInteger
		beginningWith: 'at:'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/3/2020 01:18:26'!
test044_AutocompletesEntriesFor_IncompleteKeywordMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 self firstKeyword:'
		areSelectorsOf: SmalltalkCompleterTest
		beginningWith: 'firstKeyword:'.! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 18:15:41'!
createCompleterForBrowsing: sourceCode 
	
	| browser model |
	browser := Browser new.
	browser systemCategoryListIndex: (browser systemCategoryList indexOf: (classToBrowse category)).
	browser classListIndex: (browser classList indexOf: classToBrowse name).
	
	model := (PluggableTextModel on: browser) actualContents: sourceCode.
	browser editSelection: #newMethod.

	^ self createCompleterWith: model.! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'NPM 3/18/2020 19:17:59'!
createCompleterForWorkspaceWith: sourceCode binding: aVariableName to: aValue
	
	| model |
	model := Workspace withText: sourceCode.
	(model bindingOf: aVariableName) value: aValue.
	
	^ self createCompleterWith: model! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 13:14:11'!
createCompleterWith: aModel
	
	^ (SmalltalkCompleter withModel: aModel)
		changePositionTo: aModel actualContents size;
		yourself! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 16:37:44'!
firstKeyword: a secondKeyword: b! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 16:55:18'!
m1: arg1 arg1 ! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 16:37:29'!
m2: arg1 1 + arg1 ! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 5/2/2020 16:56:16'!
m3: arg1 arg1! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 3/18/2020 19:13:43'!
autocompleteEntriesBrowsing: sourceCode
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'HAW 5/2/2020 21:51:30'!
autocompleteEntriesBrowsingMethod: aCompiledMethod 
	
	| completer selector |
	
	selector := aCompiledMethod selector.
	completer := self createCompleterForBrowsing: aCompiledMethod sourceCode.
	"Not nice, but does it work - Hernan"
	completer textProviderOrModel instVarNamed: 'currentCompiledMethod' put: aCompiledMethod.
	completer textProviderOrModel selectedMessageName: selector.
	
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 3/18/2020 19:07:19'!
autocompleteEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue 
	
	| completer |
	completer := self
		createCompleterForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
		
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 3/18/2020 18:32:34'!
browseClass: aClass 
	
	classToBrowse _ aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 20:21:58'!
assert: aCompleter analizedSelectorsFrom: classes canShowDocumentation: aBoolean detectedPossibleInvalidSelector: selectors suggested: entries 
	
	classes 
		ifNil: [ self assert: aCompleter selectorsClasses isNil ]
		ifNotNil: [ self assert: aCompleter selectorsClasses asSet = classes asSet ].
	self assert: aCompleter canShowSelectorDocumentation equals: aBoolean.
	self assert: aCompleter possibleInvalidSelectors asSet = selectors asSet.
	entries 
		ifNil: [ self assert: aCompleter entries isNil ]
		ifNotNil: [ self assert: (self entriesToCompareFrom: aCompleter entries) = (self entriesToCompareFrom: entries) ]
		
		! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 20:25:15'!
assertEntriesAreSelectorsForUnknownClasses: aCompleter 
	
	| expectedEntries expectedPossibleInvalidSelectors |
	expectedPossibleInvalidSelectors _ Set new.
	expectedEntries _ aCompleter computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: expectedPossibleInvalidSelectors.
	
	self
		assert: aCompleter
		analizedSelectorsFrom: #()
		canShowDocumentation: true
		detectedPossibleInvalidSelector: expectedPossibleInvalidSelectors
		suggested: expectedEntries! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 19:06:02'!
assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: sourceCode binding: aVariableName to: aValue
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 19:07:53'!
assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: sourceCode 
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 21:36:46'!
assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: aCompiledMethod 
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: aCompiledMethod.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 19:08:09'!
assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areSelectorsOf: aClass
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areAllSelectorsOf: aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 19:19:49'!
assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areUnaryAndBinarySelectorsOf: aClass 
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areUnaryAndBinarySelectorsOf: aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/17/2020 18:27:01'!
assertEntriesOf: aCompleter areAllSelectorsOf: aClass

	self
		assertEntriesOf: aCompleter
		areAllSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 17:38:48'!
assertEntriesOf: aCompleter areAllSelectorsOf: aClass beginningWith: aPrefix

	self assertEntriesOf: aCompleter areAllSelectorsOfAll: { aClass } beginningWith: aPrefix
! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 18:42:55'!
assertEntriesOf: aCompleter areAllSelectorsOfAll: classes beginningWith: aPrefix

	| suggested |

	suggested := classes inject: Set new into: [ :suggestedCollector :aClass |
		suggestedCollector 
			addAll: (aCompleter selectorsOf: aClass beginningWith: aPrefix);
			yourself ].
	
	self
		assert: aCompleter
		analizedSelectorsFrom: classes
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: suggested.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 20:25:15'!
assertEntriesOf: aCompleter areIdentifiersBeginningWith: aPrefix

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (aCompleter computeIdentifierEntriesBeginningWith: aPrefix)! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 20:10:35'!
assertEntriesOf: aCompleter areUnaryAndBinarySelectorsOf: aClass

	self
		assert: aCompleter
		analizedSelectorsFrom: {aClass}
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: (self using: aCompleter addUnaryAndBinarySelectorsOf: aClass).
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 19:58:16'!
assertEntriesWhenBrowsing: sourceCode areIdentifiersBeginningWith: aPrefix
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: aPrefix.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 18:55:09'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 18:58:41'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 17:40:10'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOfAll: classes
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 17:41:09'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOfAll: classes
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 18:59:22'!
assertEntriesWhenBrowsing: sourceCode areUnaryAndBinarySelectorsOf: aClass
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areUnaryAndBinarySelectorsOf: aClass.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 21:33:48'!
assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass
	
	self
		assertEntriesWhenBrowsingMethod: aMethod
		areSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 21:36:46'!
assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 21:36:46'!
assertEntriesWhenBrowsingMethod: aMethod areUnaryAndBinarySelectorsOf: aClass
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areUnaryAndBinarySelectorsOf: aClass.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 20:25:15'!
denyComputingEntriesIsSupportedWhenBrowsing: sourceCode 
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	self
		should: [ completer computeEntries ]
		raise: Error.
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: nil.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 3/18/2020 20:25:15'!
denyHasEntries: aCompleter  

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: nil
		detectedPossibleInvalidSelector: #()
		suggested: #()! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 20:24:54'!
entriesToCompareFrom: aCollectionOfEntries

	^(aCollectionOfEntries reject: [ :anEntry | 
		anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader ]) asSet! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/2/2020 20:10:35'!
using: aCompleter addUnaryAndBinarySelectorsOf: aClass

	^ aCompleter unaryAndBinarySelectorsOf: aClass beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'setUp/tearDown' stamp: 'NPM 3/18/2020 18:32:49'!
setUp

	super setUp.
	
	self browseClass: SmalltalkCompleterTest.
	
! !

!SmalltalkCompleterTest methodsFor: 'running' stamp: 'HAW 5/2/2020 20:49:50'!
performTest

	^SmalltalkCompleter changeEntriesLimitTo: SmallInteger maxVal during: [ super performTest ]! !

!SmalltalkCompleterTest class methodsFor: 'testing' stamp: 'HAW 5/2/2020 13:21:14'!
isAbstract

	^true! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:43:31'!
test023_AutocompletingMessagesFor_UnaryMessages
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + self class '.	! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:44:50'!
test024_AutocompletingMessagesFor_RightParenthesis
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 (1) '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < (1) '.! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 5/6/2020 23:45:10'!
test026_AutocompletingMessagesFor_Cascade
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class; '.
	! !

!DynamicTypingSmalltalkCompleterTest class methodsFor: 'testing' stamp: 'HAW 5/2/2020 16:27:37'!
isAbstract

	^DynamicTypingSmalltalkCompleter isForCurrentTypeSystem not! !

!TaskbarTest methodsFor: 'Running' stamp: 'jmv 10/24/2020 15:55:32'!
setUp

	needsDelete _ UISupervisor ui taskbar isNil.
	taskbar _ UISupervisor ui hideTaskbar; showTaskbar; taskbar.
	taskbar screenSizeChanged.
	taskbar world runStepMethods! !

!TaskbarTest methodsFor: 'Running' stamp: 'jmv 12/28/2017 16:12:13'!
tearDown

	needsDelete ifTrue: [
		UISupervisor ui hideTaskbar ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 12/28/2017 16:12:16'!
testClassSingleton

	self should: [ taskbar == UISupervisor ui taskbar ].
! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 9/4/2012 17:30'!
testHorizontalAlignment

	self should: [ taskbar morphPositionInWorld x = 0 ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 12/28/2017 16:12:01'!
testWidth
	
	self should: [ taskbar morphWidth = UISupervisor ui morphWidth ]! !

!TestCaseTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:47:26'!
testIsTestCase
	self assert: (TestCase new is: #TestCase).! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 3/17/2019 07:31:39'!
testShouldFailErrorsWhenNoErrorIsSignaled

	self
		should: [ self shouldFail: [] ]
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 3/17/2019 07:32:19'!
testShouldFailPassesWhenAnErrorIsSignaled

	self shouldFail: [ self error: 'an error' ]! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 4/5/2018 14:32:15'!
testShouldRaiseWithExceptionDoCanExpectException

	"This test is important becuase TestFailure is a subclass of Exception, therefore if we are expecting Exception to be raised it should not 
	catch the TestFailure - Hernan"
	
	| failureRaised |
	
	failureRaised := false.
	
	[self should: [] raise: Exception ]
		on: TestResult failure
		do: [ :failure | failureRaised := true ].
		
	self assert: failureRaised ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 1/19/2017 20:52:39'!
testShouldRaiseWithExceptionDoFailsWhenNoExceptionIsSignaled
	
	| failureSignaled |
	
	failureSignaled := false.
	
	[self 
		should: []
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evaluate this block' ]]
		on: TestResult failure
		do: [:failure | failureSignaled := true ].
		
	self assert: failureSignaled ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 1/19/2017 20:52:31'!
testShouldRaiseWithExceptionDoFailsWhenOtherExceptionTypeIsSignaled

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	[self 
		should: [ exceptionToRaise signal ]
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evalaute this block' ]]
		on: Error
		do: [:anError | self assert: exceptionToRaise equals: anError ]
		
! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 1/19/2017 20:51:53'!
testShouldRaiseWithExceptionDoPassSignaledExceptionToAssertionsBlock

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			self assert: exceptionToRaise equals: signaledException ].
		
! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 1/19/2017 20:51:45'!
testShouldRaiseWithExceptionDoValuesAssertionsBlockWhenExceptionIsRaised

	| exceptionToRaise assertionsBlockEvaluated |
	
	exceptionToRaise := Error new.
	assertionsBlockEvaluated := false.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			assertionsBlockEvaluated := true ].
		
	self assert: assertionsBlockEvaluated ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 2/6/2019 13:37:19'!
testShouldRaiseWithMessageTextDoesNotFailWithRightMessageText

	| messageText |
	
	messageText := 'some message'.
	
	self 
		shouldnt: [ 
			self 
				should: [ self error: messageText ]
				raise: Error
				withMessageText: messageText ]
		raise: TestResult failure 
		! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 2/6/2019 13:38:00'!
testShouldRaiseWithMessageTextFailsWithDifferentMessageText

	self 
		should: [ 
			self 
				should: [ self error: 'some message' ]
				raise: Error
				withMessageText: '' ]
		raise: TestResult failure 
		! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 3/17/2019 06:42:30'!
testShoulndFailErrorsWhenAnErrorIsSignaled

	self
		should: [ self shouldntFail: [ self fail ]]
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 3/17/2019 06:41:26'!
testShoulndFailPassesWhenNoErrorIsSignaled

	self shouldntFail: [ 1+2 ]! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 5/15/2019 18:33:28'!
testAssertChangesByPassesWhenActionChangesConditionByTheSpecifiedAmount

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1; add: 2 ] changes: [ aCollection size ] by: 2! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 5/15/2019 18:34:29'!
testAssertChangesFromToPassesWhenActionChangesConditionFromAndToTheSpecifiedValues

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection add: 2 ] changes: [ aCollection size ] from: 1 to: 2! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 5/15/2019 18:33:34'!
testAssertChangesPassesWhenConditionIsAlteredByAction

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1 ] changes: [ aCollection size ]! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 5/15/2019 18:33:39'!
testAssertDoeNotChangePassesWhenConditionIsNotAlteredByAction

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection sum ] doesNotChange: [ aCollection size ]! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:01'!
testAssertIsCloseToPassesForSameNumberDifferentToZero
	
	self assert: 1.5 isCloseTo: 1.5! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:12'!
testAssertIsCloseToPassesWhenBothAreZero

	self assert: 0.0 isCloseTo: 0.0! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:18'!
testAssertIsCloseToPassesWithSameCalculatedNumber
	
	self assert: 0.3 isCloseTo: 0.1 + 0.2! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:24'!
testAssertIsCloseToWithPrecisionPassesWhenDifferenceIsNegativeAndLessThanPrecision
	
	|precision newValue originalNumber |
	
	precision _ 0.1.
	originalNumber _ 0.3.
	newValue _ originalNumber - 0.01.
	
	self assert: originalNumber isCloseTo: newValue withPrecision: precision! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:30'!
testAssertIsCloseToWithPrecisionPassesWhenDifferenceIsPositiveAndLessThanPrecision
	
	|precision newValue originalNumber |
	
	precision _ 0.1.
	originalNumber _ 0.3.
	newValue _ originalNumber + 0.01.
	
	self assert: originalNumber isCloseTo: newValue withPrecision: precision! !

!TestCaseTest methodsFor: 'assert is near to' stamp: 'jmv 7/1/2019 16:24:39'!
testAssertIsNotCloseWithPrecisionPassesWhenDifferenceIsBiggerThanPrecision
	
	|precision newValue originalNumber |
	
	precision _ 0.1.
	originalNumber _ 0.3.
	newValue _ originalNumber + precision + 0.001.
	
	self assert: originalNumber isNotCloseTo: newValue withPrecision: precision! !

!TestCaseTest methodsFor: 'includes' stamp: 'HAW 5/15/2019 18:49:51'!
testAssertIncludesFailsWhenElementIsNotIncludedInCollection
	
	| collection anElement |
	
	anElement _ 1.
	collection _ #().
	
	self should: [ self assert: collection includes: anElement ] 
		 raise: TestResult failure
		 withMessageText: collection asString, ' does not include ', anElement asString! !

!TestCaseTest methodsFor: 'includes' stamp: 'HAW 5/15/2019 18:50:04'!
testAssertIncludesShouldNotFailWhenElementIsInCollection
	
	| collection anElement |
	
	anElement _ 1.
	collection _ Array with: anElement.
	
	self 
		shouldnt: [ self assert: collection includes: anElement ] 
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'should take less than' stamp: 'HAW 5/15/2019 19:08:22'!
testShouldNotTakeMoreThanFailsWhenClosureTakesMoreThanTheLimit

	self shouldFail: [ self should: [(Delay forMilliseconds: 5) wait ] notTakeMoreThan: 1 milliSeconds ]
		! !

!TestCaseTest methodsFor: 'should take less than' stamp: 'HAW 5/15/2019 19:07:04'!
testShouldNotTakeMoreThanPassesWhenClosureTakesLessThanTheLimit

	self shouldntFail: [ self should: [] notTakeMoreThan: 1 milliSeconds ]
		! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'HAW 2/10/2017 15:11:43'!
assert: expectedTestSuite hasSameTestsAs: resultTestSuite

	| expectedTests resultTests |
	
	expectedTests _ expectedTestSuite tests.
	resultTests _ resultTestSuite tests.
	
	self assert: expectedTests size equals: resultTests size.
	self assert: (expectedTests allSatisfy: [:expectedTest | resultTests anySatisfy: [:resultTest | self is: expectedTest equalTo: resultTest ]])
	! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'jmv 3/12/2018 20:41:35'!
assertIncludesThisTest: aTestSuite 
	
	| thisTestSelector |
	
	thisTestSelector _ thisContext sender selector.
	^aTestSuite tests anySatisfy: [ :aTestCase | aTestCase class = self class and: [aTestCase selector = thisTestSelector ]]! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'HAW 2/10/2017 15:20:29'!
is: expectedTest equalTo: resultTest

	^expectedTest class = resultTest class and: [ expectedTest selector =  resultTest selector ]! !

!TestSuiteTest methodsFor: 'test support' stamp: 'HAW 10/29/2019 11:13:21'!
hardCodedReferenceToSelfClass

	^TestSuiteTest ! !

!TestSuiteTest methodsFor: 'test support' stamp: 'HAW 3/3/2017 16:37:00'!
quickMethodTest

	"stub for a quick method - Hernan"! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 15:22:49'!
test01SuiteForTestCaseClassHasTheSameTestAsCreatingTheSuiteFromTheTestCaseClass

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: self class)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 15:24:17'!
test02SuiteForNoTestCaseClassUsesTestClassWithSameNameEndingWithTest

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: TestSuite)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/29/2019 11:12:11'!
test03SuiteForNoTestCaseClassAndNoTestClassLooksForReferencesInTests

	| objectTestSuite referencesToObject testCaseClasses |
	
	"I need to be sure that Object has no test class for the test to make sense - Hernan"
	self assert: self class testCaseClass isNil.
	
	objectTestSuite _ TestSuite forClass: self class.
	referencesToObject _ self class allCallsOn collect: [:aMethodReference | aMethodReference actualClass ]. 
	testCaseClasses _ (objectTestSuite tests collect: [:aTestCase | aTestCase class ]) asSet.

	self assert: objectTestSuite tests notEmpty.
	self assert: (testCaseClasses allSatisfy: [:aTestCaseClass | (aTestCaseClass is: #TestCaseClass) and: [ referencesToObject includes: aTestCaseClass ]])
	! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 15:41:52'!
test04SuiteForTestMethodIncludesOnlyTheMethod

	self assert: (TestSuite new addTest: (self class selector: thisContext selector)) hasSameTestsAs: (TestSuite forCompiledMethod: thisContext method)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 15:56:00'!
test05SuiteForNoTestMethodIncludesTestCaseSendingTheCompiledMethodSelector

	| compiledMethodSuite compiledMethodSelector senders |
	
	compiledMethodSelector _ #addTest:.
	compiledMethodSuite _ TestSuite forCompiledMethod: (TestSuite compiledMethodAt: compiledMethodSelector).
	senders _ (Smalltalk allCallsOn: compiledMethodSelector) collect: [:aMethodReference | aMethodReference selector ].
	
	self assert: compiledMethodSuite tests notEmpty.
	self assert: (compiledMethodSuite tests allSatisfy: [:aTestCase | (aTestCase class is: #TestCaseClass) and: [ senders includes: aTestCase selector]])! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 16:39:30'!
test06ForSystemCategoryWithTestCasesIncludesOnlyTestCasesClasses

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: self class category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 16:41:00'!
test07ForSystemCategoryWithNoTestCasesIncludesTestClassesTests

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: TestSuite category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 16:41:08'!
test08ForMessageCategoryContainingTestCasesIncludesThoseTestCases

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: thisContext selector) of: self class categorizedWith: classOrganizer.

	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 2/10/2017 16:38:46'!
test09ForMessageCategoryWihoutTestIsEmpty

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: #assert:hasSameTestsAs:) of: self class categorizedWith: classOrganizer.

	self assert: suite tests isEmpty! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 3/3/2017 18:53:02'!
test10CanNotDebugAsFailureQuickMethods

	self class debugAsFailure: #quickMethodTest ifCanNot: [ ^self ].
	self fail! !

!DifferenceFinderTest methodsFor: 'all' stamp: 'LC 1/24/2010 11:29'!
testCharacters
	| lcs |
	lcs := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	lcs compute.
	self
		assert: (lcs lcsAt: 1 at: 1) anyOne size = 0;
		assert: (lcs lcsAt: 1 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 4) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 5) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 2 at: 5) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 3) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 5) anyOne size = 2
! !

!DifferenceFinderTest methodsFor: 'all'!
testCharacters2
	| finder x y |
	finder := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	finder compute.
	self assert: finder differences size = 3.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: x = 'GAC'.
		y := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: y = 'AGCAT']
! !

!DifferenceFinderTest methodsFor: 'all'!
testCode
	| old new finder |
	old := 'differencesText
	| change sourceString current diff rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil
		ifTrue: ['']
		ifFalse: [current isCompiledMethod ifTrue: [(self
			formatSource: current sourceString
			inClass: current classField)
			sourceCode] ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField) ifNil: [change]) sourceCode].
	diff := TextDiffBuilder from: sourceString to: selectedString.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	diff run; printPatchSequenceOn: rtf.
	^rtf contents contents'.
	new := 'differencesText
	| change sourceString current finder rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil ifTrue: [''] ifFalse: [
		current isCompiledMethod
			ifTrue: [(self
				formatSource: current sourceString
				inClass: current classField)
				sourceCode]
			ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField)
			ifNil: [change])
			sourceCode].
	finder := DifferenceFinder wordsOf: sourceString and: selectedString.
	finder compute.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	finder differences first printTextOn: rtf.
	^rtf contents contents'.
	finder := DifferenceFinder wordsOf: old and: new.
	finder compute
! !

!DifferenceFinderTest methodsFor: 'all'!
testLines
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent
	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder linesOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]
! !

!DifferenceFinderTest methodsFor: 'all'!
testWords
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent 	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder wordsOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]
! !

!UnicodeTest methodsFor: 'set up' stamp: 'jmv 5/31/2016 11:24'!
setUp
	"
	self new setUp
	"

     bytesOfExample1 := #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
	                              16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3].
	
	"see UnicodeNotes.md"
	
     "write the bytes of an UFT8 encoded string in binary mode to a file"
	self class fileName asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: bytesOfExample1 ]! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 10/3/2015 20:03'!
test1ReadBinary
	
	"see UnicodeNotes.md"
     "
	self new setUp test1ReadBinary
	"
     | content |
	content := self class fileName asFileEntry binaryContents.
	self assert: content = bytesOfExample1! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 10/3/2015 20:04'!
test2ReadWithOutBinary
	
	"see UnicodeNotes.md"
     "
	self new setUp test2ReadWithOutBinary
	"
     | content |
	content := self class fileName asFileEntry textContents.
	self deny: content = bytesOfExample1! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 5/21/2020 19:50:04'!
test3ReadUtf8
	
	"see UnicodeNotes.md"
     "
	self new setUp test3ReadUtf8
	"
     | content byteArray |
	byteArray _ self class fileName asFileEntry binaryContents.
	content := String fromUtf8: byteArray.
	self assert: content = 'abc àè¤ ˆ‰Š'! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 10/3/2015 20:06'!
test4BackConversion
	
	"see UnicodeNotes.md"
	
     "
	self new setUp test4BackConversion
	"
     | contentInternalString contentByteArray |

	contentInternalString := String fromUtf8: self class fileName asFileEntry binaryContents.
	contentByteArray := self class fileName asFileEntry binaryContents.

	self assert: contentByteArray = (contentInternalString asUtf8: true)! !

!UnicodeTest methodsFor: 'testing' stamp: 'jmv 5/31/2016 11:24'!
test5ReadWriteUtf8
	
	"see UnicodeNotes.md"
	
     "
	self new setUp test5ReadWriteUtf8
	"
     | content byteArray byteArray2 |

	"read UTF8 Unicode file into internal string with NCRs"
	"for NCR see http://en.wikipedia.org/wiki/Numeric_character_reference"
	
	byteArray := self class fileName asFileEntry binaryContents.
	content := String fromUtf8: byteArray.
	"NCRs were added to 'content' as needed"

	"write internal string back to UTF8 file with NCRs converted back to UTF8 chars"
	self class fileName2 asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: (content asUtf8: true).  "true means: convert NCRs back to UTF8"
		].

      "compare the two versions: what is in file 'fileName' with what is in file 'fileName2'"
	byteArray := self class fileName asFileEntry binaryContents.
	byteArray2 := self class fileName2 asFileEntry binaryContents.
	self assert: byteArray = byteArray2! !

!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 19:25'!
fileName
	^'UTF8abc-test.txt'! !

!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:35'!
fileName2
	^'UTF8abc-test2.txt'! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'jmv 4/17/2013 12:11'!
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
		self deny: deltaTime < 90.
	].
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 8/17/2007 13:38'!
testValueWithinNonLocalReturnFixSimply
	"self run: #testValueWithinNonLocalReturnFixSimply"
	"The simple version to test the fix"
	self valueWithinNonLocalReturn.
	self shouldnt:[(Delay forMilliseconds: 50) wait] raise: TimedOut.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingBasic
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'jmv 11/7/2019 18:18:10'!
testValueWithinTimingNestedInner
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 200 milliSeconds.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingNestedOuter
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			3 timesRepeat: [
				[5 seconds asDelay wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 200 milliSeconds.
	! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingRepeat
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 350 milliSeconds.
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 8/17/2007 13:37'!
valueWithinNonLocalReturn
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:31:27'!
allClassCategoriesOfTestData

	^Array with: self classCategoryOfTestData ! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:31:31'!
classCategoryOfTestData
	
	"I can not call it testDataClassCategory becuase it will be taken as test!! - Hernan"
	
	^self class classCategoryOfTestData! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/18/2019 19:55:52'!
createClassNamed: aName 

	^self 
		createClassNamed: aName asSymbol "Just in case it is a string... - Hernan"
		subclassOf: RefactoringClassTestData 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/18/2019 19:55:56'!
createClassNamed: aName category: aCategory

	^self 
		createClassNamed: aName
		subclassOf: RefactoringClassTestData 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/18/2019 19:56:00'!
createClassNamed: aName instanceVariableNames: instanceVariables

	^self 
		createClassNamed: aName
		subclassOf: RefactoringClassTestData 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:31:49'!
createClassNamed: aName subclassOf: superclass 

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:31:53'!
createClassNamed: aName subclassOf: superclass category: aCategory

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:31:58'!
createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreationTest methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:32:28'!
createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables classVariableNames: classVariables poolDictionaries: poolDictionaries category: category

	self assert: (Smalltalk classNamed: aName) isNil description: 'Class ', aName, ' already exists'.
	
	classCategories add: category.
	
	^superclass
		subclass: aName
		instanceVariableNames: instanceVariables 
		classVariableNames: classVariables 
		poolDictionaries: poolDictionaries 
		category: category 
! !

!DynamicallyCodeCreationTest methodsFor: 'assertions' stamp: 'HAW 12/17/2019 10:32:57'!
assertAllClassCategoriesAreExpectedToBeRemoved

	"I'm using a halt instead of assert or error becuase I want the programmer to see what class category 
	was not defined in allClassCategoriesOfTestData - Hernan"
	(classCategories difference: self allClassCategoriesOfTestData) notEmpty ifTrue: [ self halt: #allClassCategoriesOfTestData asString, ' is not implemented correctly' ]
	! !

!DynamicallyCodeCreationTest methodsFor: 'assertions' stamp: 'HAW 12/17/2019 10:33:05'!
assertCanRunTest
	
	self assertNoTestDataClassCategoryExist.
	! !

!DynamicallyCodeCreationTest methodsFor: 'assertions' stamp: 'HAW 12/17/2019 10:33:17'!
assertNoTestDataClassCategoryExist

	self allClassCategoriesOfTestData do: [ :aClassCategory | self denyExistsClassCategory: aClassCategory ]
		! !

!DynamicallyCodeCreationTest methodsFor: 'assertions' stamp: 'HAW 12/17/2019 10:33:26'!
denyExistsClassCategory: aClassCategory
		
	self deny: (SystemOrganization hasCategory: aClassCategory) description: 'Can not run test because class category ', aClassCategory, ' already exists'! !

!DynamicallyCodeCreationTest methodsFor: 'setup/teardown' stamp: 'HAW 12/17/2019 10:33:36'!
setUp

	"If the authorInitials are not set, they will be asked - Hernan"
	UISupervisor whenUIinSafeState: [ Utilities authorInitials ].
	"Because I'm using the same system to create classes and removed them with its category at tearDown, I want to be sure 
	I will not remove something I don't have too - Hernan"
	setUpAssertionsPassed := false.
	self assertCanRunTest.
	setUpAssertionsPassed := true.
	
	classCategories := Set new.! !

!DynamicallyCodeCreationTest methodsFor: 'setup/teardown' stamp: 'HAW 12/17/2019 10:33:41'!
tearDown

	setUpAssertionsPassed ifTrue: [
		self assertAllClassCategoriesAreExpectedToBeRemoved.
		SystemOrganization removeSystemCategories: classCategories.
		"I commented this becuase it took too much time and the only ones runing these tests 
		are the ones developing refactorings that know about creating/removing test data category - Hernan
		Browser allInstancesDo: [:aBrowser | aBrowser changed: #systemCategoryList ]"]! !

!DynamicallyCodeCreationTest class methodsFor: 'class factory' stamp: 'HAW 12/17/2019 10:34:07'!
classCategoryOfTestData
	
	^ '__Refactoring-TestData__'! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 12/17/2019 11:00:59'!
correctFrom: start to: end with: aReplacement
 
	| newSourceCode |
	
	newSourceCode := sourceCode first: start - 1.
	newSourceCode := newSourceCode, aReplacement.
	newSourceCode := newSourceCode, (sourceCode copyFrom: end + 1 to: sourceCode size).
	
	sourceCode := newSourceCode ! !

!CompilerTest methodsFor: 'emulating' stamp: 'cwp 8/25/2009 20:23'!
selectFrom: start to: end 
	! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 6/18/2020 15:27:24'!
selectInvisiblyFrom: start to: stop 
! !

!CompilerTest methodsFor: 'emulating' stamp: 'cwp 8/25/2009 20:22'!
selectionInterval
	^ 1 to: 0! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 12/17/2019 10:53:27'!
text

	^ sourceCode ! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 15:44:13'!
testCanNotDefineVarsInsideBackTick

	self 
		should: [ self class compile: 'm1 `|v1| v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 16:08:17'!
testCanNotHaveMoreThanOneExpressionInsideBackTick

	self 
		should: [ self class compile: 'm1 `1 factorial. 1 factorial`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'backtick expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 16:09:28'!
testCanNotHaveReturnInsideBackTick

	self 
		should: [ self class compile: 'm1 `^10`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 15:41:08'!
testCanNotReferenceSelfInsideBackTick

	self 
		should: [ self class compile: 'm1 `self`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'self')) ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 15:41:13'!
testCanNotReferenceSuperInsideBackTick

	self 
		should: [ self class compile: 'm1 `super`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'super')) ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 15:45:04'!
testCanNotReferenceTempVarsInsideBackTick

	self 
		should: [ self class compile: 'm1 |v1| `v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'Can not evaluate code') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 6/23/2020 15:41:18'!
testCanNotReferenceThisContextInsideBackTick

	self 
		should: [ self class compile: 'm1 `thisContext`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'thisContext')) ]! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'EB 12/20/2019 20:54:26'!
testAddsMoreThanOneUndeclaredVariableCorrectlyInBlock
	
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ var1 := 1. goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [ | goo var1 | var1 := 1. goo := 1. ^goo + var1 ]' equals: sourceCode
! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:15:47'!
testAddsUndeclaredVariableCorrectlyInBlockWhenNoSpaceBetweenLastOneAndPipe

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|var1| var1 := 1.goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [|var1 goo | var1 := 1.goo := 1.^ goo + var1]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'EB 1/27/2020 00:10:26'!
testAddsUndeclaredVariableCorrectlyInBlockWhenTheresAnArgumentAlready

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'EB 1/27/2020 02:46:02'!
testAddsUndeclaredVariableCorrectlyInBlockWhenTheresArgumentsAndTempDeclarationPipesWithNoTemps

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'EB 12/20/2019 20:55:29'!
testAddsUndeclaredVariableCorrectlyInMethodAndInBlock

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle var1 := 1. [ goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle | var1 |', String newLineString, 'var1 := 1. [ | goo | goo := 1. ^goo + var1 ]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:15:27'!
testAddsUndeclaredVariableCorrectlyInMethodWhenNoSpaceBetweenLastOneAndPipe

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle |var1| var1 := 1.goo := 1.^ goo + var1'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle |var1 goo | var1 := 1.goo := 1.^ goo + var1' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:18:11'!
testAddsUndeclaredVariableInRightBlock
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|goo| goo := 1. ^goo ]. [goo1:=1.^ goo1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [|goo| goo := 1. ^goo ]. [ | goo1 |goo1:=1.^ goo1]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'EB 1/27/2020 00:31:46'!
testAddsUndeclaredVariableWithAnArgumentAndTempAlreadyInBlock

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | foo | foo := 2. goo := 1.^ goo + var1 + foo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [ :var1 | | foo goo | foo := 2. goo := 1.^ goo + var1 + foo ]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:06:40'!
testDoesNotConfusesVariablesWithSameNameOfDifferentBlocksWhenRemoving

	| testClass counter |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]. [ | goo | ]'.
	counter := 0.
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			counter := counter + 1.
			unusedVariable resume: counter = 1 ].
		
	self assert: 'griffle [  ]. [ | goo | ]' equals: sourceCode.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:07:39'!
testDoesNotRemoveUnusedVariableIfSaidSo

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: false ].
		
	self assert: 'griffle | goo |' equals: sourceCode.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:12:44'!
testKeepsVariableDefinitionWhenThereAreUsedTemporaries

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo a | a := 1. ^a'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: 'griffle | a | a := 1. ^a' equals: sourceCode.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:03:46'!
testRemovesUnusedVariablesFromBlocks

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: 'griffle [  ]' equals: sourceCode.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:01:28'!
testRemovesVariableDefinitionWhenNoMoreUnusedTemporaries

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: 'griffle ' equals: sourceCode.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:13:33'!
testReusesBlockTempDeclarationWhenEmptyForUndeclaredVariable

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ || goo := 1.^ goo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: 'griffle [ | goo | goo := 1.^ goo ]' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:12:52'!
testReusesMethodTempDeclarationWhenEmptyForUndeclaredVariable

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle || goo := 1.^ goo'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareMethodTempAction ].
		
	self assert: 'griffle | goo | goo := 1.^ goo' equals: sourceCode! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 17:12:34'!
testUndeclaredVariableIsSignaledForUndeclaredVariables

	| testClass |
	
	testClass := self createTestClass.
	
	self 
		should:  [ testClass compile: 'griffle ^ goo' notifying: self ]
		raise: UndeclaredVariable
		withExceptionDo: [ :anUndeclaredVariable |
			self assert: 'goo' equals: anUndeclaredVariable name ]! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:09:35'!
testUndefinedVariableIsSignaledForVariablesDefinedButNotInitialized

	| testClass |
	
	testClass := self createTestClass.

	self 
		should: [testClass compile: 'griffle | goo | ^ goo' notifying: self]
		raise: UndefinedVariable
		withExceptionDo: [ :anUndefinedVariable |
			self assert: 'goo' equals: anUndefinedVariable name ]! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:10:53'!
testUnknownSelectorIsSignalForMessagesSendNotImplemented

	| testClass unknowSelector |
	
	testClass := self createTestClass.
	unknowSelector := 'reallyHopeThisIsntImplementedAnywhere'.
	
	self 
		should: [ testClass
				compile: 'griffle self ', unknowSelector
				notifying: self]
		raise: UnknownSelector
		withExceptionDo: [ :anUnknowSelector |
			self assert: unknowSelector equals: anUnknowSelector name ]! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'HAW 12/17/2019 11:11:49'!
testUnusedVariableIsSignaledForUnusedVariables

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	self 
		should: [testClass compile: sourceCode notifying: self]
		raise: UnusedVariable
		withExceptionDo: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name ]
! !

!CompilerTest methodsFor: 'private' stamp: 'HAW 12/17/2019 10:38:10'!
createTestClass
	
	^self createClassNamed: #CompilerTestDataClass__! !

!CompilerTest methodsFor: 'private' stamp: 'cwp 8/25/2009 20:28'!
unusedVariableSource
	^ 'griffle 
		| goo |
		^ nil'! !

!RefactoringTest methodsFor: 'assertions' stamp: 'HAW 6/1/2017 19:01:00'!
assertCreation: aCreationBlock failsWith: aMessageTextCreator

	self 
		should: aCreationBlock 
		raise: self refactoringError
		withMessageText: aMessageTextCreator ! !

!RefactoringTest methodsFor: 'assertions' stamp: 'HAW 6/1/2017 14:30:58'!
assertCreation: aCreationBlock warnsWith: aMessageTextCreator 

	self 
		should: aCreationBlock 
		raise: self refactoringWarning
		withMessageText: aMessageTextCreator ! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 16:38:44'!
canNotRefactorDueToReferencesRefactoringError
	
	^ Refactoring canNotRefactorDueToReferencesErrorClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:58:26'!
refactoringError

	^ Refactoring refactoringErrorClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 12:05:52'!
refactoringWarning

	^ Refactoring refactoringWarningClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 12/18/2019 16:02:50'!
referencesRefactoringWarning

	^ Refactoring referencesWarningClass - MessageNotUnderstood.! !

!AddInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 6/11/2017 18:12:55'!
classToRefactorName

	^#ClassToAddInstVar! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:17'!
test01NewVariableNameCanNotBeEmpty
	
	newVariable := '   '.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]
! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:21'!
test02NewVariableNameCanNotContainBlanks
	
	newVariable := 'a b'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]

	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:25'!
test03NewVariableNameCanNotContainInvalidCharacters

	newVariable := '2a'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]

! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 6/11/2017 18:38:49'!
test04ValidNewVariableNameGetBlanksTrimmed

	newVariable := ' a '.
			
	self assert: newVariable withBlanksTrimmed equals: (AddInstanceVariable named: newVariable to: self class) newVariable! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:31'!
test05NewVariableNameCanNotBeDefinedInClass

	| classToRefactor |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:43'!
test06NewVariableNameCanNotBeDefinedInSuperclasses

	|  classToRefactorSuperclass classToRefactor  |

	newVariable := 'a'.
	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]
! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:42:47'!
test07NewVariableNameCanNotBeDefinedInAnySubclass

	| classToRefactor classToRefactorSubclass |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 12:33:14'!
test08AddCreatesNewInstanceVariable

	| classToRefactor add | 
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.

	add := AddInstanceVariable named: newVariable to: classToRefactor.
	add apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:43:31'!
test09FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:43:45'!
test10FailsWhenNewVariableEqualsArgumentOfAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:44:12'!
test11FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:44:28'!
test12FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:44:46'!
test13FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass

	| selector classToRefactor subclass |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:43:17'!
test14NewVariableNameCanNotBeAReservedName

	| classToRefactor |

	classToRefactor := self createClassNamed: self classToRefactorName.
	
	ClassBuilder reservedNames do: [ :aReservedName | 
		self 
			assertCreation: [AddInstanceVariable named: aReservedName to: classToRefactor ]
			failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariableCanNotBeAReservedName: aReservedName ]]
		! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test01AddingParameterToUnaryMessageAddsColonAndParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue equals: (senderMethod methodNode block statements first arguments first literalValue) printString


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test02AddingParameterToKeywordMessageAddsNewKeywordAndParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test03AddingParameterToKeywordMessageMultipleSendsInSameMethodSameLiteral

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' 1.'.
	classToRefactor compile: senderSelector asString, collaboration, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test04AddingParameterToKeywordMessageWithMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test05AddingParameterToKeywordMessageWithBinaryMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size + 4.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test06AddingParameterToKeywordMessageWithKeywordMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' (self at: 1 put: 2).'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test07AddingParameterToUnaryMessageChangesReferencesToSelector

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' #', oldSelector asString, ' size'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test08AddingParameterToKeywordMessageChangesReferencesToSelector

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' #', oldSelector asString, ' size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	

! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 12:24:53'!
test09FailsIfNewParameterValueCanNotBeCompiled

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '+' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueDoesNotCompileErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 12:25:16'!
test10FailsIfNewParameterValueIsEmpty

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: ' ' toUnarySelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueCanNotBeEmptyErrorMessage ]
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 16:57:23'!
test11NewParameterNameCanNotBeEmpty

	| newParameter |
	
	newParameter := ' '.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter withBlanksTrimmed ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 12:34:07'!
test12NewParameterNameCanNotStartWithNumber

	| newParameter |
	
	newParameter := '1x'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 12:34:28'!
test13NewParameterNameCanNotContainSpaces

	| newParameter |
	
	newParameter := 'an Integer'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 15:14:45'!
test14NewParameterCanNotBeDefinedAsInstanceVariableInAnyImplementor

	| classToRefactor oldSelector newParameter  |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newParameter.
	classToRefactor compile: oldSelector asString.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: oldSelector 
			implementors: {classToRefactor>>oldSelector} senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameter: newParameter definedAsInstanceVariableIn: {classToRefactor} ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 9/2/2018 19:59:27'!
test15NewParameterCanNotBeDefinedAsParameterInAnyImplementor

	| classToRefactor oldSelector newParameter implementors |
	
	oldSelector := 'm1:' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, newParameter.
	implementors := {classToRefactor>>oldSelector}.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' using: 'm2:' asSymbol toKeywordSelector: oldSelector 
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameterDefinedAsLocal: newParameter ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 12:21:52'!
test16WhenCreatedForUnarySelectorMustBeUnary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: 'm1:' asSymbol 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeUnaryErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 12:27:42'!
test17WhenCreatedForBinarySelectorMustBeBinary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:' asSymbol toKeywordSelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeKeywordErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 9/4/2018 20:10:41'!
test18CanNotAddParameterToBinaryKeyword

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' at: 1 addingLast: true initializedWith: '1' to: '+' asSymbol implementing: 'm1:' asSymbol
			addingToImplementors: '' addingToSenders: ''
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter selectorCanNotBeBinaryErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 15:48:13'!
test19AllImplementorsMustImplementOldSelector

	| implementors |
	
	implementors := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: (thisContext selector, 'x') asSymbol
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidImplementors: implementors ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 15:53:37'!
test20AllSendersShouldSendOldSelector

	| oldSelector invalidSenders |
	
	oldSelector := thisContext selector.
	invalidSenders := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: oldSelector
			implementors: {thisContext method} senders: invalidSenders ]
		 failsWith: [ AddParameter errorMessageForInvalidSenders: invalidSenders of: oldSelector ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 16:24:22'!
test21NewKeywordToAddToOldSelectorCanNotBeUnary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 16:26:55'!
test22NewKeywordToAddToOldSelectorCanNotBeBinary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: '+' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 16:27:58'!
test23NewKeywordToAddToOldSelectorCanNotBeKeywordWithMoreThanOneParameter

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:m2:' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/24/2018 17:10:04'!
test24AddingParameterToSendersTakeCaresOfSeparators

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, Character newLineCharacter asString,'	'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 5/24/2019 10:06:10'!
test25IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForUnaryMessages

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 5/24/2019 10:06:40'!
test26IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForKeywordMessages

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 8/25/2018 11:23:42'!
test27NewParameterValueCanNotHaveMoreThanOneStatement

	self 
		assertCreation: [ AddParameter 
			named: 'newParameter' initializedWith: 'self m1. self m2.' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		failsWith: [ AddParameter newParameterValueCanNotHaveMoreThanOneStatementErrorMessage ].
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 5/24/2019 10:07:01'!
test28WhenSenderLastParameterIsABlockAddedParameterIsNotAddedInsideTheBlock

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange senderFirstParameterValue |

	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	senderFirstParameterValue := '[ 1 factorial ]'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, senderFirstParameterValue.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first arguments first closureCreationNode ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: senderFirstParameterValue equals: senderSourceCode
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:42:08'!
test29AddingParameterAsFirstOneWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm2:m1:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1'.
	classToRefactor compile: senderSelector asString, ' self m1: 1'.
	
	refactoring := AddParameter 
		named: newParameter at: 1 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode arguments at: 1) name equals: newParameter.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments first literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m2: 2 m1: 1'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:45:09'!
test30AddingParameterInTheMiddleWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, ' self m1: 1 m3: 3'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode arguments at: 2) name equals: newParameter.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m1: 1 m2: 2 m3: 3'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:45:01'!
test31AddingParameterInTheMiddleWithManySendersWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, '
	self m1: 1 m3: 3.
	self
		m1: 4
		m3: 6'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderMethod sourceCode equals: senderSelector asString, '
	self m1: 1 m2: 2 m3: 3.
	self
		m1: 4
		m2: 2 m3: 6'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:44:50'!
test32IndexCanNotBeLessThanOne
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 0
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 0 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:44:43'!
test33IndexCanNotBeBiggerThanNumberOfParametersPlusOne
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 3
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 3 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:44:36'!
test34IndexMustBeInteger
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 1.5
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 1.5 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:44:23'!
test35AddingParameterRetractsInsertionPointWhenAtInsertionPointIsANewLine

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, Character newLineCharacter asString, Character newLineCharacter asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 1/7/2019 13:44:13'!
test36ParentesisAreAddedToSendersOfUnaryMessage

	self shouldFail: [ | refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ', self size'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self assert: senderSelector asString, ' (self ', oldSelector asString, '), self size' equals: (classToRefactor>>senderSelector) sourceCode 
	]! !

!AddParameterTest methodsFor: 'class factory' stamp: 'HAW 8/24/2018 17:20:56'!
classToRefactorName
	
	^#ClassToAddParameter! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:18:58'!
test01CannotChangeSelectorOrderInUnaryMessages

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1 to: #m1 implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:18:58'!
test02CannotChangeSelectorOrderInBinaryMessages

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #+ to: #+ implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 06:18:58'!
test03CannotChangeSelectorOrderInKeywordMessagesWithOneParameterOnly

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1: to: #m1: implementors: #() senders: #() ]
		 failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 05:44:03'!
test04ChangesKeywordsOrder

	| classToRefactor refactoring oldSelector newSelector |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	classToRefactor := self createClassNamed: #ClassToChangeSelectorOrder.
	classToRefactor compile: (self selectorAndParametersFor: oldSelector).

	refactoring := ChangeKeywordsSelectorOrder from: oldSelector to: newSelector implementors: { classToRefactor >> oldSelector } senders: #().
	refactoring apply.

	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector)! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 05:52:51'!
test05ChangesParametersOrder

	| classToRefactor refactoring oldSelector newSelector renamedMethod parameters selectorAndParameters |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	classToRefactor := self createClassNamed: #ClassToChangeSelectorOrder.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	classToRefactor compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder from: oldSelector to: newSelector implementors: { classToRefactor >> oldSelector } senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters first)
		equals: renamedMethod methodNode argumentNames ! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 05:31:03'!
test06NewSelectorMustIncludeOldSelectorKeywords

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1:m2: to: #m3:m1: implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage ]
		! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 3/17/2019 05:52:00'!
test07ChangesKeywordsAndParametersOrderForMoreThanTwoKeywords

	| classToRefactor refactoring renamedMethod oldSelector newSelector selectorAndParameters parameters |

	oldSelector := #m1:m2:m3:.
	newSelector := #m2:m3:m1:.
	classToRefactor := self createClassNamed: #ClassToChangeSelectorOrder.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	classToRefactor compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder from: oldSelector to: newSelector implementors: { classToRefactor >> oldSelector } senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters third with: parameters first)
		equals: renamedMethod methodNode argumentNames ! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'source code creation' stamp: 'HAW 3/17/2019 05:43:44'!
selectorAndParametersFor: aKeywordSelector

	^String streamContents: [ :stream |
		aKeywordSelector keywords
			withIndexDo: [ :aKeyword :index |
				stream
					nextPutAll: aKeyword;
					space;
					nextPut: $p;
					print: index ]
			separatedBy: [  stream space ]]
				! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 9/28/2019 00:49:10'!
assertClassHasDefined: aSelector withBody: newSourceCode

	self
		assert: newSourceCode
		equals: (classToRefactor >> aSelector) sourceCode! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 6/23/2019 18:30:02'!
assertClassHasDefined: aSelector withBody: newSourceCode inCategory: aCategory

	self assertClassHasDefined: aSelector withBody: newSourceCode.
	self assert: aCategory equals: (classToRefactor >> aSelector) category! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 1/12/2020 21:08:55'!
assertExtracting: codeToExtract from: originalCode named: newMessage defines: newCode andUpdates: updatedCode

	| originalSelector category |
	category _ #category.
	originalSelector _ classToRefactor compile: originalCode classified: category.

	(ExtractMethod
		fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
		of: classToRefactor >> originalSelector
		to: newMessage
		categorizedAs: category) apply.

	self
		assertClassHasDefined: originalSelector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newCode inCategory: category! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:10:35'!
creationWithMessage: aMessage onInterval: anIntervalToExtract ofMethod: methodToExtractCodeFrom failsWith: aRefactoringExceptionMessageEvaluationBlock errorCondition: anExceptionHandlingCondition

	self
		should: [
			ExtractMethod
				fromInterval: anIntervalToExtract
				of: methodToExtractCodeFrom
				to: aMessage
				categorizedAs: Categorizer default ]
		raise: anExceptionHandlingCondition
		withMessageText: aRefactoringExceptionMessageEvaluationBlock! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:13:01'!
creationWithSelectorNamed: aSelectorName onInterval: anIntervalToExtract ofMethod: aMethodToRefactor failsWith: aRefactoringErrorMessageEvaluationBlock errorCondition: anExceptionHandlingCondition 

	self
		creationWithMessage: (Message selector: aSelectorName)
		onInterval: anIntervalToExtract
		ofMethod: aMethodToRefactor
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: anExceptionHandlingCondition! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:09:14'!
tryingToExtract: someCode from: anExistingSourceCode failsWith: aRefactoringErrorMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:11:49'!
tryingToExtract: someCode from: anExistingSourceCode using: aMessage failsWith: aRefactoringErrorMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:12:03'!
tryingToExtract: someCode from: anExistingSourceCode using: aMessage raisesWarning: aRefactoringWarningMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:09:47'!
tryingToExtractOnInterval: anIntervalToExtract failsWith: aRefactoringErrorMessageEvaluationBlock

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: anIntervalToExtract
		ofMethod: self methodToExtractCodeFrom
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:10:02'!
tryingToExtractWithSelectorNamed: aSelectorName failsWith: aRefactoringErrorMessageEvaluationBlock

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 5/25/2020 01:08:00'!
tryingToExtractWithSelectorNamed: aSelectorName raisesWarning: aRefactoringWarningMessageEvaluationBlock

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning
		! !

!ExtractMethodTest methodsFor: 'class factory' stamp: 'RNG 4/20/2019 21:31:05'!
classToRefactorName

	^ #ClassToExtractMethod! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 5/12/2019 23:21:30'!
compileMethodToExtractCodeFrom

	classToRefactor compile: self sourceCodeOfMethodToExtractCodeFrom! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 5/12/2019 23:22:01'!
methodToExtractCodeFrom

	^ classToRefactor >> self selectorOfMethodToExtractCodeFrom! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 5/12/2019 23:25:05'!
selectorOfMethodToExtractCodeFrom

	^ #mExtractMethodExample:! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 9/8/2019 18:40:40'!
sourceCodeOfMethodToExtractCodeFrom

	^ self selectorOfMethodToExtractCodeFrom, ' anArgument

	<anAnnoyingPragma>
	| localVar |
	<anotherAnnoyingPragma>
	self m1.
	self m2: ''hey true''.
	self m3: anArgument , ''^ 21''.
	localVar _ Set with: ''hello''.
	localVar := #($x $y $z) size and: [ (9) * 3 ].
	^ localVar + ((4))'! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'RNG 5/8/2020 21:06:17'!
intervalOf: aPieceOfSourceCode locatedIn: sourceCode

	| interval |
	interval _ sourceCode intervalOfSubCollection: aPieceOfSourceCode.
	^ (interval first to: interval last - 1) asSourceCodeInterval! !

!ExtractMethodTest methodsFor: 'set up' stamp: 'RNG 9/28/2019 01:16:48'!
setUp

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName.
	self compileMethodToExtractCodeFrom.! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test09ExtractingALiteralCreatesANewMethodAndChangesExistingCodeToCallThatNewMethod

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '4'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test15ExtractingAListOfStatementsCreatesANewMethodWithoutReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'self m3.
	self m4: 5'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	', codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test17AStringContainingTheReturnCharacterCanBeExtracted
	"this is to make sure the return is found using AST logic, not by text"

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '''^ 3'' size'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test18ExtractingAListOfStatementsIncludingDotsRemovesThemAndContinuesWithMethodExtraction

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '
	self m3.
	self m4: 5.'.
	originalCode _ 'm1
	' , codeToExtract.
	newMethodCode _ 'm2

	self m3.
	self m4: 5'.
	updatedCode _ 'm1
	
	self m2.'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test19ASelectionContainingAdditionalParenthesesAtTheBeginningAndOrEndCanBeExtracted

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '((42))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 42'.
	updatedCode _ 'm1 ^ ((self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test20ASelectionContainingALocalVariableIsParameterizedOnTheExtractedMessage

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'local + 3'.
	originalCode _ 'm1 | local | ^ ' , codeToExtract.
	newMethodCode _ 'm2: local

	^ ' , codeToExtract.
	updatedCode _ 'm1 | local | ^ self m2: local'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('local'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test21ASelectionContainingAdditionalParenthesesOnOneSideCanBeExtractedAndItIsNotTrimmed

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(9) * 3'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test22ItIsPossibleToExtractTheLastExpressionOfAMethod

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'localVar + ((4))'.
	originalCode _ 'm1 | localVar | ^ ' , codeToExtract.
	newMethodCode _ 'm2: localVar

	^ ' , codeToExtract.
	updatedCode _ 'm1 | localVar | ^ self m2: localVar'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('localVar'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test26ItIsPossibleToExtractAnEntireBlockIncludingALocalAssignment

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '[ |something| something _ 3 ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test27ItIsPossibleToExtractExpressionsWithOptimizedSelectors

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 ifNil: [ #boo ] ifNotNil: [ #yay ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:08:55'!
test29ExtractingABinaryExpressionToAKeywordMessageIntroducesParenthesesToNotBreakOriginalPrecedence

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 + arg'.
	originalCode _ 'm1: arg ^ ' , codeToExtract , ' * 3'.
	newMethodCode _ 'm2: arg

	^ ' , codeToExtract.
	updatedCode _ 'm1: arg ^ (self m2: arg) * 3'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('arg'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 1/12/2020 21:11:36'!
test30ItIsPossibleToExtractAQuotedExpression

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '`3 + 4`'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'HAW 3/30/2020 16:16:37'!
test31ItIsPossibleToExtractExpressionsWithOptimizedSelectorsWhereTheReceiverIsNotASimpleLiteral

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 factorial ifNotNil: [ #boo ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 4/5/2020 23:19:13'!
test32ItIsPossibleToExtractATemporaryDeclarationOfABlockIfItIsNotUsedOutsideOfSelectionInterval

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ^ [ ' , codeToExtract , ' ]'.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 ^ [ self m2 ]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 4/5/2020 23:19:23'!
test33ItIsPossibleToExtractATemporaryDeclarationIfItIsNotUsedOutsideOfSelectionInterval

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 4/14/2020 20:52:00'!
test36ItIsPossibleToExtractACascadeExpression

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(3 + 2) factorial; yourself'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 4/24/2020 20:32:48'!
test37ItIsPossibleATempDeclarationAlongWithABlock

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| temp | []'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'RNG 5/4/2020 20:08:01'!
test38ItIsPossibleToExtractCodeThatContainsMultipleParenthesisWithSpacesBetweenThem

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(  (3 + 4))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 3 + 4'.
	updatedCode _ 'm1 ^ (  (self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test01NewSelectorShouldNotBeEmpty

	self
		tryingToExtractWithSelectorNamed: #''
		failsWith: [ NewSelectorPrecondition newSelectorCanNotBeEmptyErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test02NewSelectorShouldNotContainSeparators

	self
		tryingToExtractWithSelectorNamed: #'my selector'
		failsWith: [ NewSelectorPrecondition newSelectorCanNotContainSeparatorsErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test03ExtractingToSelectorAlreadyDefinedInTheClassRaisesAWarning

	self
		tryingToExtractWithSelectorNamed: self selectorOfMethodToExtractCodeFrom
		raisesWarning: [
			NewSelectorPrecondition
				warningMessageFor: self selectorOfMethodToExtractCodeFrom
				isAlreadyDefinedIn: classToRefactor ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test04NewSelectorShouldNotStartWithANumber

	self
		tryingToExtractWithSelectorNamed: #'2selector'
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test05NewUnarySelectorShouldNotStartWithASymbol

	self
		tryingToExtractWithSelectorNamed: #-
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:03:37'!
test06IntervalToExtractIsNotBeforeMethodSourceCodeBoundaries

	self
		tryingToExtractOnInterval: (-1 to: 2) asSourceCodeInterval
		failsWith: [ ExtractMethod outOfBoundsSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:03:45'!
test07IntervalToExtractIsNotAfterMethodSourceCodeBoundaries

	self
		tryingToExtractOnInterval: (1 to: self sourceCodeOfMethodToExtractCodeFrom size + 2) asSourceCodeInterval
		failsWith: [ ExtractMethod outOfBoundsSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 21:51:01'!
test08IntervalToExtractShouldNotContainAReturnExpression

	self
		tryingToExtract: '^ localVar + ((4))'
		from: 'm1 ^ localVar + ((4))'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeIncludingAReturnStatement ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:48:14'!
test10IntervalToExtractDoesNotContainsPartOfTheOriginalSelector

	self
		tryingToExtract: 'm1 ^ 8'
		from: 'm1 ^ 8'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 22:55:57'!
test11IntervalToExtractDoesNotContainsPartOfLocalVariablesDefinition

	self
		tryingToExtract: '| localVar |'
		from: 'm1 | localVar | ^ localVar + 3'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:48:14'!
test12IntervalToExtractDoesNotContainsPartOfPragmasDefinition

	self
		tryingToExtract: '<annoyingPragma>'
		from: 'm1 <annoyingPragma>'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:48:14'!
test13IntervalToExtractShouldBeAValidSmalltalkExpression

	self
		tryingToExtract: '+ ((4))'
		from: 'm1 ^ 3 + ((4))'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 22:15:26'!
test14ItIsNotPossibleToExtractTheLeftSideOfAnAssignment

	| firstOccurrence secondOccurrence intervalToExtract |
	firstOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: 1.
	secondOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: firstOccurrence + 1.
	intervalToExtract := secondOccurrence toSelfPlus: 'localVar' size.
	self
		tryingToExtractOnInterval: intervalToExtract asSourceCodeInterval
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingLeftSideOfAssignment ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:48:13'!
test16CanNotExtractPartOfALiteral

	self
		tryingToExtract: 'true'
		from: 'm1 ^ ''hey true'''
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:04:30'!
test23CanNotExtractWithAnInvalidInterval

	self
		tryingToExtractOnInterval: (10 to: 9) asSourceCodeInterval
		failsWith: [ ExtractMethod noSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:26:46'!
test24ItIsNotPossibleToExtractAnIntervalIncludingATemporaryAssignmentExpression

	self
		tryingToExtract: 'localVar _ 8'
		from: 'm1 | localVar | localVar _ 8'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test25NewUnarySelectorShouldNotContainInvalidCharacters

	self
		tryingToExtractWithSelectorNamed: 'hola)' asSymbol
		failsWith: [ NewSelectorPrecondition invalidCharacterInsideNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:30:44'!
test28ItIsNotPossibleToExtractALocalVariableInsideOfItsDeclaration

	self
		tryingToExtract: 'localVar'
		from: 'm1 | localVar | ^ #foo'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:02:48'!
test34TryingToExtractAMethodWithLessArgumentsThanNeededFails

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1: arguments: #('localVar1'))
		failsWith: [ ExtractMethod wrongNumberOfArgumentsGivenErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:02:52'!
test35TryingToExtractAMethodWithMoreArgumentsThanNeededFails

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1:m2:m3: arguments: #('localVar1' 'localVar2' 'localVar3'))
		failsWith: [ ExtractMethod wrongNumberOfArgumentsGivenErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 19:13:15'!
test39TryingToExtractToAMethodThatExistsInASuperclassRaisesAWarning

	self
		tryingToExtract: '42'
		from: 'm1 ^ 42'
		using: (Message selector: #yourself)
		raisesWarning: [ NewSelectorPrecondition warningMessageFor: #yourself isAlreadyDefinedIn: Object ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 5/25/2020 00:37:19'!
test40CanNotExtractTemporaryAssignmentWithDeclarationIfTheVariableIsUsedOutsideOfSelectionInterval

	self
		tryingToExtract: '|var| var _ 42.'
		from: 'm1 |var| var _ 42. ^ var'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 15:35:08'!
test41CannotExtractPartOfMethodSignature

	self
		tryingToExtract: 'arg'
		from: 'm1: arg ^ self'
		using: (Message selector: #m2: arguments: #('arg'))
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!ExtractToTemporaryTest methodsFor: 'class factory' stamp: 'RNG 2/22/2020 21:20:46'!
classToRefactorName

	^ #ClassToDoExtractTemporary! !

!ExtractToTemporaryTest methodsFor: 'set up' stamp: 'RNG 3/24/2020 22:30:57'!
setUp

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName! !

!ExtractToTemporaryTest methodsFor: 'test helpers' stamp: 'RNG 3/24/2020 22:42:02'!
assertExtracting: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring

	self
		assertExtracting: codeToExtract
		from: sourceCodeOfMethodToRefactor
		toVariableNamed: newVariable
		updatesTo: sourceCodeAfterRefactoring
		usingLeftArrowAssignment: true! !

!ExtractToTemporaryTest methodsFor: 'test helpers' stamp: 'RNG 5/9/2020 15:10:44'!
assertExtracting: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring usingLeftArrowAssignment: leftArrowAssignmentPreference

	| intervalToExtract methodToRefactor actualSourceCodeAfterRefactoring applyRefactoring |
	classToRefactor compile: sourceCodeOfMethodToRefactor.
	intervalToExtract _ self intervalOf: codeToExtract locatedIn: sourceCodeOfMethodToRefactor.
	methodToRefactor _ classToRefactor >> #m1.

	applyRefactoring := [ actualSourceCodeAfterRefactoring := (ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor) apply ].
	
	Preferences
		withTemporaryValue: leftArrowAssignmentPreference
		of: #leftArrowAssignmentsInGeneratedCode
		do: applyRefactoring.

	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'test helpers' stamp: 'RNG 5/8/2020 21:06:24'!
intervalOf: aPieceOfSourceCode locatedIn: sourceCode

	| interval |
	interval _ sourceCode intervalOfSubCollection: aPieceOfSourceCode.
	^ (interval first to: interval last - 1) asSourceCodeInterval! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 2/25/2020 16:28:15'!
test12ItIsPossibleToExtractASingleLiteralFromAMethodWithoutAnyTempsOrArguments

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ 42'.
	sourceCodeAfterRefactoring _ 'm1

	| new |
	new _ 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 2/25/2020 16:32:52'!
test13ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAlreadyOtherTemporary

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	| four |
	four _ 4.
	^ four + 2'.
	sourceCodeAfterRefactoring _ 'm1

	| four two |
	four _ 4.
	two _ 2.
	^ four + two'.

	self assertExtracting: '2' from: sourceCode toVariableNamed: 'two' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 3/24/2020 18:04:28'!
test14ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAnEmptyTemporariesDeclaration

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	| |
	^ 3 factorial'.
	sourceCodeAfterRefactoring _ 'm1

	| f |
	f _ 3 factorial.
	^ f'.

	self assertExtracting: '3 factorial' from: sourceCode toVariableNamed: 'f' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 3/24/2020 22:43:08'!
test15ItIsPossibleToExtractCodeAndGenerateAssignmentUsingAnsiAssignmentStyle

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ 42'.
	sourceCodeAfterRefactoring _ 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring usingLeftArrowAssignment: false! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 5/4/2020 19:30:06'!
test16ItIsPossibleToExtractASingleLiteralFromABlockWithoutAnyTempsOrArguments

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ [ self m2 ]'.
	sourceCodeAfterRefactoring _ 'm1

	^ [ | new |
	new _ self m2 ]'.

	self assertExtracting: 'self m2' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 4/12/2020 23:32:37'!
test19ItIsPossibleToExtractAnIntervalWithSomeExtraSpaces

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ 42'.
	sourceCodeAfterRefactoring _ 'm1

	| new |
	new _ 42.
	^ new'.

	self assertExtracting: ' 42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 4/24/2020 20:56:58'!
test20ItIsPossibleToExtractAnEmptyBlock

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ []'.
	sourceCodeAfterRefactoring _ 'm1

	| new |
	new _ [].
	^ new'.

	self assertExtracting: '[]' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 4/25/2020 13:24:55'!
test22ItIsPossibleToExtractACascadeExpression

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	^ 3 factorial; yourself'.
	sourceCodeAfterRefactoring _ 'm1

	| new |
	new _ 3 factorial; yourself.
	^ new'.

	self assertExtracting: '3 factorial; yourself' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'RNG 5/4/2020 19:11:34'!
test24WhenExtractingAnEntireStatementItOnlyGeneratesTheVariableAssignment

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode _ 'm1

	self run'.
	sourceCodeAfterRefactoring _ 'm1

	| action |
	action _ self run'.

	self assertExtracting: 'self run' from: sourceCode toVariableNamed: 'action' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:13:05'!
test01NewVariableCanNotBeEmpty

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ ''.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ 2'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:13:01'!
test02NewVariableHasToBeValid

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ 'a b'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:12:58'!
test03NewVariableNameCanNotBeDefinedInMethod

	| newVariable sourceCodeToExtract sourceCode intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 | ', newVariable, ' | ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodToRefactor methodNode ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:01:16'!
test04FailsIfNewTemporaryIsEqualToInstanceVariableInClass

	| newVariable sourceCode sourceCodeToExtract methodToRefactor intervalToExtract |
	newVariable _ 'new'.
	classToRefactor addInstVarName: newVariable.
	sourceCodeToExtract _ '78'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 5/24/2020 20:01:16'!
test05FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass

	| newVariable sourceCode sourceCodeToExtract methodToRefactor superclassToRefactor intervalToExtract |
	newVariable _ 'new'.
	superclassToRefactor _ self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor superclass: superclassToRefactor.
	sourceCodeToExtract _ '78'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 17:10:14'!
test06SourceCodeToExtractCanNotIncludeReturn

	| newVariable sourceCode sourceCodeToExtract  intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '^ 1'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeIncludingAReturnStatement ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:12:44'!
test07SourceCodeToExtractCanNotBeEmpty

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ ' '.
	sourceCode _ 'm1 ^ 1'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeToExtractCanNotBeEmpty]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 16:53:38'!
test08SourceCodeToExtractCanNotHaveSyntaxErrors

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor expectedSyntaxErrorMessage |
	newVariable _ 'new'.
	sourceCodeToExtract _ '1 +'.
	sourceCode _ 'm1 ^ 1 + 3'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	 
	[ Parser parse: sourceCodeToExtract class: classToRefactor noPattern: true.
		self fail. ]
		on: SyntaxErrorNotification 
		do: [ :anError | expectedSyntaxErrorMessage _ anError messageText].
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasSyntaxError: expectedSyntaxErrorMessage ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 16:18:15'!
test09SourceCodeToExtractHasToBeOneStatement

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '1+2. 3+4'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasToBeOneStatement ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:12:27'!
test10FailsIfIntervalToExtractIsBeforeMethodSourceCodeBoundaries

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (-1 to: 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 2/25/2020 15:22:19'!
test11FailsIfIntervalToExtractIsAfterMethodSourceCodeBoundaries

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (sourceCode size - 1 to: sourceCode size + 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 18:47:52'!
test17CannotExtractPartOfMethodName

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm1'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 16:20:08'!
test18CannotExtractPartOfMessageSend

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ self m2'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm2'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 16:20:48'!
test21CannotExtractLeftSideOfAssignment

	| newVariable sourceCode methodToRefactor intervalToExtract |
	newVariable := 'new'.
	sourceCode := 'm1 | existing | existing := 2'.
	classToRefactor compile: sourceCode.
	intervalToExtract := (17 to: 24) asSourceCodeInterval. "second occurrence of 'existing' variable "
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 4/26/2020 15:23:20'!
test23CannotUseAReservedNameAsTheNewTemporaryVariable

	| intervalToExtract methodToRefactor newVariable sourceCode |

	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		sourceCode := 'm1 ^ 2'.
		classToRefactor compile: sourceCode.
		intervalToExtract := self intervalOf: '2' locatedIn: sourceCode.
		methodToRefactor := classToRefactor >> #m1.

		self 
			assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 7/12/2020 18:48:18'!
test25CannotExtractArgumentNamesFromMethodSignature

	| intervalToExtract methodToRefactor newVariable sourceCode |
	newVariable := 'new'.
	sourceCode := 'm1: arg ^ self'.
	classToRefactor compile: sourceCode.
	intervalToExtract := self intervalOf: 'arg' locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1:.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:01:00'!
test01ChangesTheSuperclassOfTheClassToRefactor

	| classToRefactor refactoring oldSuperclass newSuperclass |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	oldSuperclass := classToRefactor superclass.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass equals: classToRefactor superclass.
	self assert: (newSuperclass subclasses includes: classToRefactor).
	self assert: oldSuperclass equals: newSuperclass superclass.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:02:51'!
test02ClassToRefactorStructureIsMaintained

	| classToRefactor refactoring instanceVariables category classVariables poolDictionaries |
	
	instanceVariables := 'instVar1 instVar2'.
	classVariables := 'ClassVar1 ClassVar2'.
	poolDictionaries := 'SharedAAA'.
	category :=  self classCategoryOfTestData.
	
	[ Smalltalk at: poolDictionaries asSymbol put: Dictionary new.
	classToRefactor := self 
		createClassNamed: #ClassToAddSuperclass
		subclassOf: RefactoringClassTestData 
		instanceVariableNames: instanceVariables
		classVariableNames: classVariables
		poolDictionaries: poolDictionaries
		category: category.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	refactoring apply.
	
	self assert: instanceVariables equals: classToRefactor instanceVariablesString.
	self assert: classVariables equals: classToRefactor classVariablesString.
	self assert: poolDictionaries equals: classToRefactor sharedPoolsString.
	self assert: category equals: classToRefactor category ] ensure: [ Smalltalk removeKey: poolDictionaries asSymbol ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:17:48'!
test03NewSuperclassCategoryIsTheSameAsClassToRefactorCategory

	| classToRefactor refactoring classToRefactorCategory newSuperclass |

	classToRefactorCategory := self classCategoryOfTestData.
	classToRefactor := self createClassNamed: #ClassToAddSuperclass category: classToRefactorCategory.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: classToRefactorCategory equals: newSuperclass category.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:19:28'!
test04NewSuperclassHasNoVariables

	| classToRefactor refactoring classToRefactorCategory newSuperclass |

	classToRefactorCategory := self classCategoryOfTestData.
	classToRefactor := self createClassNamed: #ClassToAddSuperclass category: classToRefactorCategory.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass instVarNames isEmpty.
	self assert: newSuperclass classVarNames isEmpty.
	self assert: newSuperclass sharedPools isEmpty.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:21:16'!
test05NewSuperclassShouldNotExist

	| classToRefactor existingClass existingClassName |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	existingClassName := #AlreadyExistingClass.
	existingClass := self createClassNamed: existingClassName.
	
	self
		assertCreation: [ InsertSuperclass to: classToRefactor named: existingClassName ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistClassNamed: existingClass name ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:22:13'!
test06NewSuperclassNameHasToBeASymbol

	self 
		assertCreation: [ InsertSuperclass to: self class named: 'aString' ]
		failsWith: [ NewClassPrecondition newNameMustBeSymbolErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 5/24/2019 10:09:41'!
test07NewSuperclassNameHasToStartWithUppercaseLetter
	
	self
		assertCreation: [ InsertSuperclass to: self class named: #_A ]
		failsWith: [ NewClassPrecondition newNameMustStartWithRightLetterErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:22:31'!
test08WhenAppliedToMetaclassWorksAsWithClass

	| classToRefactor refactoring newSuperclass |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	
	refactoring := InsertSuperclass to: classToRefactor class named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass equals: classToRefactor superclass.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:22:56'!
test09NewSuperclassNameCanNotBeEmpty

	self 
		assertCreation: [ InsertSuperclass to: self class named: '' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotBeEmptyErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 09:13:27'!
test10NewSuperclassNameCanNotHaveSpaces
	
	self
		assertCreation: [ InsertSuperclass to: self class named: 'With spaces' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotHaveSeparatorsErrorMessage ].! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 12/8/2019 18:08:54'!
test01CannotMoveWhenReferencingInstanceVariables
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: referencedVariable.
	classToRefactor compile: selector asString,' ^', referencedVariable.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod referencingInstanceVariablesErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 12/8/2019 18:43:15'!
test02CannotMoveToClassWhenLocalVariableIsTheSameAsClassInstanceVariable
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class addInstVarName: referencedVariable.
	classToRefactor compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 12/8/2019 23:35:40'!
test03CannotMoveToInstanceWhenLocalVariableIsTheSameAsClassInstanceVariable
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor addInstVarName: referencedVariable.
	classToRefactor class compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor class >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 12/8/2019 23:44:00'!
test04InstanceMethodIsMovedToClassMethod
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor class includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 12/8/2019 23:51:21'!
test05ClassMethodIsMovedToInstanceMethod
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor class >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor class includesSelector: selector).
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'class factory' stamp: 'LMY 12/8/2019 17:35:59'!
classToRefactorName

	^#ClassToMoveMethod! !

!PushDownInstanceVariableTest methodsFor: 'class factory' stamp: 'MSC 12/21/2019 09:49:48'!
classToRefactorName
	^#ClassToPushInstanceVariableDown.! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:13:23'!
test01AnInstanceVariableDoesNotExistOnClassToFactorCannotBePushedDownToSubclasses
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushDownCreationFailsWith: PushDownInstanceVariable instanceVariableDoesNotExistOnClassToRefactor .
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:10:51'!
test02AnInstanceVariableThatIsAccessedOnClassToRefactorCannotBePushedDownToAllSubclasses

	|  selector |
	
	selector := #newMethod.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	classToRefactor compile: selector, '^ ', instanceVariableToPushDown.
	
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	self assertPushDownCreationFailsWith: (PushDownInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushDown 
		isAccessedInMethodsOf: classToRefactor).
	
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown)].! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:49:31'!
test03AnInstanceVariableThatIsNotAccessedOnClassToRefactorShouldBePushedDownToAllSubclasses

	|  pushDown |
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	pushDown := PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor.
	pushDown apply.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self assert: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].! !

!PushDownInstanceVariableTest methodsFor: 'setup' stamp: 'MSC 12/21/2019 22:01:42'!
setUp

	super setUp.
	
	instanceVariableToPushDown := 'a'.! !

!PushDownInstanceVariableTest methodsFor: 'assertions' stamp: 'HAW 12/26/2019 19:41:11'!
assertPushDownCreationFailsWith: aMessageTextCreator

	self
		assertCreation: [ PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor. ]
		failsWith: aMessageTextCreator ! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'fz 12/4/2019 14:09:00'!
test01AnEmptyMethodOnClassToRefactorShouldBePushedDownToSubclasses

	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor selector push |

	selector := #newMethod.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: #Subclass1 subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: #Subclass2 subclassOf: classToRefactor.
	
	classToRefactor compile: selector asString.
           
	push := PushDownMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSubclass1 includesSelector: selector).
	self assert: (classToRefactorSubclass2 includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	
	! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'HAW 12/14/2019 11:23:35'!
test02AMethodTemporaryVarDeclaredAsInstVarOnASubclassCanNotBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor selector newVariable |
	
	selector := #newMethod.
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: #Subclass1 subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: #Subclass2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	classToRefactor compile: selector, '|', newVariable, '|'.
           
	self
		assertCreation: [ PushDownMethod for: classToRefactor >> selector ]
		 failsWith: [ PushDownMethod errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary ].
	   
	self deny: (classToRefactorSubclass1 includesSelector: selector).
	self deny: (classToRefactorSubclass2 includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'HAW 12/14/2019 11:24:14'!
test03AMethodArgumentDeclaredAsInstVarOnASubclassCanNotBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor selector newVariable |
	
	selector := #newMethod:.
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: #Subclass1 subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: #Subclass2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	classToRefactor compile: selector, newVariable.
           
	self
		assertCreation: [ PushDownMethod for: classToRefactor >> selector ]
		 failsWith: [ PushDownMethod errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary].
	   
	self deny: (classToRefactorSubclass1 includesSelector: selector).
	self deny: (classToRefactorSubclass2 includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'fz 12/4/2019 14:58:23'!
test04AMethodAccessesClassToRefactorInstVarShouldBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable selector push |

	selector := #newMethod.
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: #Subclass1 subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: #Subclass2 subclassOf: classToRefactor.
	
	classToRefactor compile: selector, '^ 1 + ', newVariable.
           
	push := PushDownMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSubclass1 includesSelector: selector).
	self assert: (classToRefactorSubclass2 includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'fz 12/4/2019 14:58:27'!
test05AMethodWritesClassToRefactorInstVarShouldBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable selector push |

	selector := #newMethod.
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: #Subclass1 subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: #Subclass2 subclassOf: classToRefactor.
	
	classToRefactor compile: selector, ' a = 1'.
           
	push := PushDownMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSubclass1 includesSelector: selector).
	self assert: (classToRefactorSubclass2 includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushDownMethodTest methodsFor: 'tests' stamp: 'HAW 12/14/2019 11:24:59'!
test06AMethodInClassToRefactorExistsInSubClassesShouldOverrideTheExistingSubClassesMethod
	
	| classToRefactor classToRefactorSubclass newVariable selector subclassMethodContent |
	
	newVariable := 'a'.	
	selector := #newMethod.
	subclassMethodContent := selector, '
	^ 2 + ', newVariable, '.'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactor compile: subclassMethodContent.
	 
	classToRefactorSubclass := self createClassNamed: #Subclass subclassOf: classToRefactor.
	classToRefactorSubclass compile: selector, '^ 1 + ', newVariable.

	self 
		assertCreation: [ PushDownMethod for: classToRefactor >> selector ]
		warnsWith: [ PushDownMethod warningMesssageForMessageAlreadyImplementedIn: { classToRefactorSubclass } ].
		
	self assert: (classToRefactorSubclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'fz 12/4/2019 14:05:35'!
classToRefactorName	

	^#ClassToPushMethodDown! !

!PushUpInstanceVariableTest methodsFor: 'class factory' stamp: 'MSC 12/15/2019 12:44:48'!
classToRefactorName
	^#ClassToPushInstanceVariableUp.! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:18:51'!
test01AnInstanceVariableThatIsNotUsedOnClassToRefactorShouldBePushedUpToSuperclass

	|  pushUp |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.

	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:18:52'!
test02AnInstanceVariableOnMultipleSubclassesShouldBeRemovedFromAllSubclassesAndPushedUpToSuperClass

	|  pushUp  |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	self createClassNamed: #AnoterSubClassWithInstVar subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.
	
	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:13:23'!
test03AnInstanceVariableDoesNotExistOnClassToRefactorCannotBePushedUp
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushUpCreationFailsWith: PushUpInstanceVariable instanceVariableDoesNotExistOnClassToRefactor.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/26/2019 20:34:42'!
test04AnInstanceVariableThatIsUsedOnSuperClassAsTemporaryVariableCannotBePushedUp

	| selector |
	
	selector := #newMethod.
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithVariableAsTemporaryVariable.
	classToRefactorSuperClass compile: selector, '| ' , instanceVariableToPushUp , ' |'.
	
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	
	self assertPushUpCreationFailsWith: (PushUpInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushUp 
		isDefinedInMethodsOf: classToRefactorSuperClass).
	
	self deny: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).! !

!PushUpInstanceVariableTest methodsFor: 'setup' stamp: 'MSC 12/21/2019 22:12:16'!
setUp

	super setUp.
	
	instanceVariableToPushUp := 'a'.! !

!PushUpInstanceVariableTest methodsFor: 'assertions' stamp: 'HAW 12/26/2019 20:18:51'!
assertPushUpCreationFailsWith: aMessageTextCreator

	self
		assertCreation: [ PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor. ]
		failsWith: aMessageTextCreator.! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:47:47'!
test01AnEmptyMethodOnClassToRefactorShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector push |

	selector := #newMethod.
   
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector asString.
           
	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:47:51'!
test02AMethodAccessesClassToRefactorInstVarCanNotBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		 failsWith: [ PushUpMethod errorMessageForMethodCannotAccessInstanceVariable ].
	   
	self deny: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:47:54'!
test03AMethodWritesClassToRefactorInstVarCanNotBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, ' a = 1'.

	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		 failsWith: [ PushUpMethod errorMessageForMethodCannotAccessInstanceVariable ].
	   
	self deny: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:47:57'!
test04AMethodWritesSuperClassInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' a = 1'.	

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:48:00'!
test05AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 11/12/2019 11:48:03'!
test06AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 12/14/2019 11:25:39'!
test07AMethodInClassToRefactorExistsInSuperClassShouldOverrideTheExistingSuperClassMethod
	
	| classToRefactor classToRefactorSuperclass newVariable selector superclassMethodContent |
	
	newVariable := 'a'.	
	selector := #newMethod.
	superclassMethodContent := selector, '
	^ 2 + ', newVariable, '.'.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.
	classToRefactorSuperclass compile: selector, '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: superclassMethodContent.

	self 
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		warnsWith: [ PushUpMethod methodToPushUpExistOnSuperclassWarningMessage ].
		
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'class factory' stamp: 'MSC 11/12/2019 11:48:09'!
classToRefactorName	

	^#ClassToPushMethodUp! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 16:15:11'!
test01NoVariableIsRemovedWhenClassHasNoInstanceVariable

	| classToRefactor refactoring removedInstanceVariables |
	
	classToRefactor := self createClassNamed: #ClassWithoutInstVar.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: removedInstanceVariables isEmpty! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 16:15:14'!
test02UnreferencedVariablesAreRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariable |
	
	unreferencedVariable := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: unreferencedVariable.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariable)
	! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 16:15:18'!
test03VariablesWithReferencesAreNotRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactor compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	
	
! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 16:22:44'!
test04VariablesWithReferencesInSubclassesAreNotRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName classToRefactorSubclass |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactorSubclass := self createClassNamed: #ClassWithReferenceInstVar subclassOf: classToRefactor.
	classToRefactorSubclass compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	
! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 6/12/2017 19:10:08'!
test01CanNotRemoveAnInstanceVariableNotDefinedInClass

	| variableToRemove classToRefactor |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithoutInstVar.
	
	self 
		assertCreation: [ RemoveInstanceVariable named: variableToRemove from: classToRefactor ]
		failsWith: [ RemoveInstanceVariable errorMessageForInstanceVariable: variableToRemove notDefinedIn: classToRefactor ].
		
	
	! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 15:24:17'!
test02CanNotRemoveInstanceVariableWithReferences

	| variableToRemove classToRefactor selector |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: variableToRemove.
	selector := #m1.
	classToRefactor compile: selector,' ^', variableToRemove.
	
	self 
		assertCreation: [ RemoveInstanceVariable named: variableToRemove from: classToRefactor ]
		failsWith: [ RemoveInstanceVariable errorMessageForInstanceVariable: variableToRemove isReferencedInAll: (Array with: classToRefactor>>selector) ].
		
	
	! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/2/2018 15:24:28'!
test03WhenAppliedRemovesInstanceVariable

	| variableToRemove classToRefactor remove |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: variableToRemove.
	
	remove := RemoveInstanceVariable named: variableToRemove from: classToRefactor.
	remove apply.
	
	self deny: (classToRefactor definesInstanceVariableNamed: variableToRemove)	
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 1/2/2020 06:58:49'!
test01CanNotRemoveParameterFromUnaryMessages

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: 'aParameter' 
			from: #m1
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromUnaryMessagesErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 1/2/2020 06:59:13'!
test02CanNotRemoveParameterFromBinaryMessages

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: '+' asSymbol 
			from: #+ 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromBinaryMessagesErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/2/2018 19:46:52'!
test03CanNotRemoveParameterNotInMessage

	| classToRefactor selectorToRemoveParameterFrom parameterNotInMessage |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			named: parameterNotInMessage 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {classToRefactor >> selectorToRemoveParameterFrom } 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterNotInMessage: parameterNotInMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/2/2018 19:46:03'!
test04AllImplementorsMustImplementSelector

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementors |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	implementors := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementors 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForInvalidImplementors: implementors ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/2/2018 19:49:28'!
test05AllSendersShouldSendSelector

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove invalidSenders |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	invalidSenders := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {} 
			senders: invalidSenders ]
		failsWith: [ RemoveParameter errorMessageForInvalidSenders: invalidSenders of: selectorToRemoveParameterFrom ]
	
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/2/2018 19:57:35'!
test06NoImplementorReferencesParameterToRemove

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementorsReferencingParameter |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter ^parameter'.
	implementorsReferencingParameter := {classToRefactor>>selectorToRemoveParameterFrom }.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementorsReferencingParameter 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterToRemoveIsReferenced: parameterToRemove ]! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/3/2018 17:58:11'!
test07FirstParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep) ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/3/2018 18:45:48'!
test08LastParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToKeep := 'm1:'.
	keywordToRemove := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToKeep, keywordToRemove) asSymbol. 
	parameterToKeep := 'parameter1'.
	parameterToRemove := 'parameter2'.
	classToRefactor compile: keywordToKeep,parameterToKeep, ' ', keywordToRemove, parameterToRemove,'
	| t1 |'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep,' 
	| t1 |') ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/4/2018 14:34:22'!
test09MiddleParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring newSelector |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := ('m0:',keywordToRemove, keywordToKeep) asSymbol. 
	newSelector := ('m0:', keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: 'm0: parameter0 
	',keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: newSelector).
	self assert: (classToRefactor sourceCodeAt: newSelector) equals: ('m0: parameter0 
	',keywordToKeep, parameterToKeep) ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 9/3/2018 17:34:00'!
test10ParameterFromOneKeywordSelectorIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	classToRefactor compile: selectorToRemoveParameterFrom asString,parameterToRemove.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: 'm1' asSymbol).! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:29:53'!
test11ParameterOfSendersOfMoreThanOneKeywordSelectorsIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self 
			', keywordToRemove, ' 1 
			', keywordToKeep, ' 2 '.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			', keywordToKeep, ' 2 ')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:30:01'!
test12ParameterOfSendersOfOneKeywordSelectorsIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom,parameterToRemove.
	classToRefactor compile: senderSelector asString,'
		self 
			', selectorToRemoveParameterFrom, '
			 (1+2).'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: 'm1' asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			m1.')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 3/4/2019 09:22:15'!
test13ParameterIsRemovedCorrecltyFromSendersWithMoreThanOneSend

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self ', keywordToRemove, ' (1+1) ', keywordToKeep, ' 2.
		self ', keywordToRemove, ' 3 ', keywordToKeep, ' 4.'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsOrRefersTo: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self ', keywordToKeep, ' 2.
		self ', keywordToKeep, ' 4.')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 1/1/2020 23:33:19'!
test14CanNotRemoveParameterIndexLessThanOne

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 0
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 1/1/2020 23:36:16'!
test15CanNotRemoveParameterIndexBiggerThanOldSelectorNumberOfKeywords

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 2
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 1/1/2020 23:38:57'!
test16ParameterIndexMustBeInteger

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1.5
			named: parameterNotInMessage 
			from: #m1:m2: 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'class factory' stamp: 'HAW 9/1/2018 12:24:23'!
classToRefactorName
	
	^#ClassToRemoveParameter! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/1/2017 19:44:41'!
test01NewClassNameHasToBeDifferentToOldOne

	self 
		assertCreation: [ RenameClass from: self class to: self class name ]
		failsWith: [ RenameClass newNameEqualsOldNameErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:36:20'!
test02NewClassNameHasToBeASymbol

	self 
		assertCreation: [ RenameClass from: self class to: 'aString' ]
		failsWith: [ NewClassPrecondition newNameMustBeSymbolErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 5/24/2019 10:09:47'!
test03NewClassNameHasToStartWithUppercaseLetter

	self 
		assertCreation: [ RenameClass from: self class to: #_A ]
		failsWith: [ NewClassPrecondition newNameMustStartWithRightLetterErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:33:08'!
test04NewClassShouldNotExist

	| newClassName |
	
	newClassName := #Object.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistClassNamed: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 12/13/2018 17:59:19'!
test04_01NewClassNameShouldNotBeAGlobalVariable

	| newClassName |
	
	newClassName := #Smalltalk.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistGlobalNamed: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:33:35'!
test05NewClassShouldNotBeUndeclare

	| newClassName undeclared |
	
	newClassName := #UndeclareClass.
	undeclared := Dictionary new.
	undeclared at: newClassName put: nil.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk undeclared: undeclared ]
		failsWith: [ NewClassPrecondition errorMessageForNewClassIsUndeclared: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/4/2017 19:12:10'!
test06OldClassIsRenamedToNewClass

	| oldClassName newClassName classToRefactor rename |

	oldClassName :=  'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	self assert: (Smalltalk classNamed: oldClassName) isNil.
	self deny: (Smalltalk classNamed: newClassName) isNil.! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/4/2017 18:18:12'!
test07OldClassDirectReferencesAreRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteralThorough: newClassName).
	self deny: (referencingMethod hasLiteralThorough: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/4/2017 18:19:04'!
test08OldClassLiteralReferencesAreRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest08' asSymbol.
	newClassName := 'NewClassTest8' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteralThorough: newClassName).
	self deny: (referencingMethod hasLiteralThorough: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/4/2017 18:36:22'!
test09OtherClassDirectReferencesAreNotRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest09' asSymbol.
	newClassName := 'NewClassTest09' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. Object new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteralThorough: newClassName).
	self deny: (referencingMethod hasLiteralThorough: oldClassName).
	self assert: (referencingMethod hasLiteralThorough: #Object).
! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 6/4/2017 19:07:19'!
test10OtherLiteralReferencesAreNotRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest10' asSymbol.
	newClassName := 'NewClassTest10' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size. #Object size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteralThorough: newClassName).
	self deny: (referencingMethod hasLiteralThorough: oldClassName).
	self assert: (referencingMethod hasLiteralThorough: #Object).
! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 16:30:56'!
test11OldClassDirectAndLiteralReferencesAreRenameAtOnce

	| oldClassName newClassName classToRefactor selector rename renamedReferences |
	
	oldClassName := 'OldClassTest11' asSymbol.
	newClassName := 'NewClassTest11' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	renamedReferences := rename apply.
	
	self assert: 1 equals: renamedReferences size.
	self assert: selector equals: renamedReferences anyOne selector.
	self assert: newClassName equals: renamedReferences anyOne classSymbol.
	self assert: ((classToRefactor sourceCodeAt: selector) includesSubString: '#', newClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 4/4/2018 19:56:09'!
test12CanNotRenameAMetaclass

	self 
		assertCreation: [ RenameClass from: self class class to: #Object ]
		failsWith: [ RenameClass classToRenameCanNotBeMetaclassErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/13/2018 18:35:38'!
test13NewClassNameHasToBeASymbol

	self 
		assertCreation: [ RenameClass from: self class to: '' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotBeEmptyErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 09:20:07'!
test14NewClassNameCanNotHaveSpaces

	| oldClassName classToRefactor |

	oldClassName :=  'OldClassTest14' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.

	self 
		assertCreation: [ RenameClass from: classToRefactor to: 'With spaces' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotHaveSeparatorsErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 14:44:13'!
test15ItDoesNotRenameReferencesToClassVariableWithSameName

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences |

	oldClassName :=  'OldClassTest15' asSymbol.
	newClassName := 'NewClassTest15' asSymbol.
	"First I create a class var with the same name as the class to rename and a reference to it - Hernan"
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldClassName.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 14:45:20'!
test16ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest16' asSymbol.
	newClassName := 'NewClassTest16' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 14:46:34'!
test17ItDoesNotRenameReferencesToClassVariableWithSameNameAsNewClassDefinedInAnySuperclass

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest17' asSymbol.
	newClassName := 'NewClassTest17' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: newClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', newClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 16:21:35'!
test18WorksCorrectlyWhenSourceCodeEndsWithClassToRename

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest18' asSymbol.
	newClassName := 'NewClassTest18' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ^', oldClassName asString.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteralThorough: newClassName).
	self deny: (referencingMethod hasLiteralThorough: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 16:24:09'!
test19RenamesClassWhenMethodReferencesClassAndDoesNothing

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	"Currently the rename class is not handling this case becuase message #allCallsOn: does not return a method
	that references a class but does nothing becuase it is optimize to return self... it is a really wierd case so I decided
	no to solve it by now - Hernan"
	self shouldFail: [
		oldClassName := 'OldClassTest18' asSymbol.
		newClassName := 'NewClassTest18' asSymbol.
		classToRefactor := self createClassNamed: oldClassName.
		selector := #m1.
		classToRefactor compile: selector, ' ', oldClassName asString.
	
		rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
		rename apply.
	
		referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteralThorough: newClassName).
		self deny: (referencingMethod hasLiteralThorough: oldClassName)] ! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 17:53:53'!
define: anOldName with: aGlobalValue toBeRenamedAs: aNewName while: aBlock

	| oldNameAsSymbol newNameAsSymbol |
	
	oldNameAsSymbol := anOldName asSymbol.
	newNameAsSymbol := aNewName asSymbol.
	
	self assert: (Smalltalk at: oldNameAsSymbol ifAbsent: [ nil ]) isNil.
	self assert: (Smalltalk at: newNameAsSymbol ifAbsent: [ nil ]) isNil.
	
	[ 
		Smalltalk at: oldNameAsSymbol put: aGlobalValue.
		aBlock value: oldNameAsSymbol value: newNameAsSymbol value: aGlobalValue ] ensure: [
			Smalltalk removeKey: oldNameAsSymbol ifAbsent: [].
			Smalltalk removeKey: newNameAsSymbol ifAbsent: []].
		! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:20:03'!
test00GlobalCanNotBeClass

	self 
		assertCreation: [ RenameGlobal from: #Object to: 'NewObject__' asSymbol ]
		failsWith: [ RenameGlobal globalCanNotBeClassErrorMessage]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:20:37'!
test01NewNameHasToBeDifferentToOldOne

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'OldNameToRename__' 
		while: [ :oldName :newName :globalValue |
			self 
				assertCreation: [ RenameGlobal from: oldName to: newName ]
				failsWith: [ RenameGlobal newNameEqualsOldNameErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:26:40'!
test02NewNameHasToBeASymbol

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: newName asString ]
			failsWith: [ NewGlobalPrecondition newNameMustBeSymbolErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:27:23'!
test03NewNameShouldNotBeAClass

	| className |
	
	className := #Object.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: className in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistClassNamed: className ]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:27:33'!
test04NewNameShouldNotBeAGlobalVariable

	| globalName |
	
	globalName := #Smalltalk.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: globalName in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistGlobalNamed: globalName ]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:27:45'!
test06OldNameIsRenamedToNewName

	| rename |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		self assert: (Smalltalk at: oldName ifAbsent: [nil]) isNil.
		self assert: (Smalltalk at: newName) equals: globalValue ].! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:27:58'!
test07OldDirectReferencesAreRenamed

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteralThorough: newName).
		self deny: (referencingMethod hasLiteralThorough: oldName).
		self assert: (classToRefactor new perform: selector) equals: globalValue ].
		
			
	
! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:28:09'!
test08OldNameLiteralReferencesAreRenamed

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^#', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteralThorough: newName).
		self deny: (referencingMethod hasLiteralThorough: oldName).
		self assert: (classToRefactor new perform: selector) equals: newName ].
		
			! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:28:21'!
test09NewNameHasToBeASymbol

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: '' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotBeEmptyErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:28:30'!
test10NewNameCanNotHaveSpaces

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: 'With spaces' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotHaveSeparatorsErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 18:33:27'!
test11ItDoesNotRenameReferencesToClassVariableWithSameName

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 18:33:34'!
test12ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass

	| classWithClassVariable rename renamedReferences superClassOfClassWithClassVarReference oldNameAsString |

	oldNameAsString := 'OldNameRenamed__'.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldNameAsString asSymbol.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:29:03'!
test13OldNameMustBeDefined
	
	self 
		assertCreation: [ RenameGlobal from: 'AGlobal__' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal errorMessageForGlobalNotDefined: 'AGlobal__' asSymbol ]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:34:06'!
test14ItDoesNotRenameReferencesToClassVariableFromClassSideWithSameName

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable class compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:49:43'!
test15OldNameCanNotBeEmpty
	
	self 
		assertCreation: [ RenameGlobal from: '' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal oldNameCanNotBeEmptyErrorMessage ]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 3/26/2019 19:26:58'!
withDefaultsDo: aBlock

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'NewNameToRename__' 
		while: aBlock! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 6/11/2017 18:13:05'!
classToRefactorName

	^#ClassToRenameInstVar! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 6/1/2017 14:28:02'!
createClassToRefactor
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: oldVariable ! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 5/28/2017 21:36:47'!
createClassToRefactorAndAssertRenameCreationFailsWith: aMessageTextCreator

	self createClassToRefactor.
	self assertRenameCreationFailsWith: aMessageTextCreator ! !

!RenameInstanceVariableTest methodsFor: 'assertions' stamp: 'HAW 6/1/2017 19:00:37'!
assertRenameCreationFailsWith: aMessageTextCreator

	self
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: aMessageTextCreator ! !

!RenameInstanceVariableTest methodsFor: 'setup' stamp: 'HAW 5/28/2017 21:33:44'!
setUp

	super setUp.
	
	oldVariable := 'old'.
	newVariable := 'new'.! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/27/2017 11:21:06'!
test01CanNotRenameAnInstanceVariableNotDefinedInClass

	classToRefactor := self createClassNamed: #ClassWithoutInstVar instanceVariableNames: ''.
	
	self assertRenameCreationFailsWith: [ RenameInstanceVariable errorMessageForInstanceVariable: oldVariable notDefinedIn: classToRefactor ]
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:01'!
test02NewVariableNameCanNotBeEmpty
	
	newVariable := '   '.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:05'!
test03NewVariableNameCanNotContainBlanks
	
	newVariable := 'a b'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:10'!
test04NewVariableNameCanNotContainInvalidCharacters

	newVariable := '2a'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:33:30'!
test05ValidNewVariableNameGetBlanksTrimmed

	newVariable := ' a '.
	self createClassToRefactor.
			
	self
		shouldnt: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		raise: self refactoringError ! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:17'!
test06NewVariableNameCanNotBeDefinedInClass

	newVariable := oldVariable.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:22'!
test07NewVariableNameCanNotBeDefinedInSuperclasses

	|  classToRefactorSuperclass  |

	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: oldVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]
	
	
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/3/2019 08:45:39'!
test08NewVariableNameCanNotBeDefinedInAnySubclass

	| classToRefactorSubclass |

	self createClassToRefactor.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/27/2017 11:36:03'!
test09RenameCreatesNewInstanceVariableAndDeletesOldOne

	| rename |

	self createClassToRefactor.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor definesInstanceVariableNamed: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:05:15'!
test10ReadReferencesToOldVariableAreRenamed

	| selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ^' , oldVariable.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:07:27'!
test11WriteReferencesToOldVariableAreRenamed

	|  selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ' , oldVariable, ' := 10'.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method writesInstanceVariable: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:10:59'!
test12ReferencesInSubclassesAreRenamed

	| selector method classToRefactorSubclass rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactorSubclass _ self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: ''.
	classToRefactorSubclass compile: selector , ' ' , oldVariable, ' := 10. ^' , oldVariable.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactorSubclass >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable).
	self deny: (method writesInstanceVariable: oldVariable) ! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:46:36'!
test13ReferencesToOtherVariablesAreNotRenamed

	| variableToKeep selector method rename |

	variableToKeep := 'keep'.
	selector := #m1.
	classToRefactor := self createClassNamed:  self classToRefactorName instanceVariableNames: oldVariable, ' ', variableToKeep.
	classToRefactor compile: selector , ' ' , variableToKeep, ' := ' , oldVariable, '. ^' , variableToKeep.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: variableToKeep).
	self assert: (method writesInstanceVariable: variableToKeep) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 5/28/2017 21:15:59'!
test14NewVariableOfExistingInstancesReferToOldVariableValue

	| rename instance |

	self createClassToRefactor.
	instance := classToRefactor new.
	instance instVarNamed: oldVariable put: 1.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: 1 equals:  (instance instVarNamed: newVariable).
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:45:27'!
test15FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:45:44'!
test16FailsWhenNewVariableEqualsArgumentOfAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:45:59'!
test17FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:46:25'!
test18FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/7/2019 22:46:39'!
test19FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass

	| selector subclass |

	self createClassToRefactor.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:55:59'!
test20ClassChangeIsAddedWhenRenamingAnInstanceVariable

	| rename classChanges refactorClassChangeSet refactorClassChangeSets |

	"I have to do this because on this case I want to keep track of the changes - Hernan"
	[ classToRefactor := self 
		createClassNamed: self classToRefactorName 
		subclassOf: Object 
		instanceVariableNames: oldVariable 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

	refactorClassChangeSets := ChangeSet allChangeSetsWithClass: classToRefactor.
	self assert: 1 = refactorClassChangeSets size description: 'Can not run the test because the class is in more than one changeset (or in none)'.
	refactorClassChangeSet := refactorClassChangeSets anyOne.
	refactorClassChangeSet removeClassChanges: classToRefactor.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	classChanges := refactorClassChangeSet classChangeAt: classToRefactor name.
	self assert: (classChanges includes: #change). ]
		ensure: [ | package | 
			classToRefactor class compile: 'wantsChangeSetLogging ^false'.
			ChangeSet allChangeSets remove: refactorClassChangeSet.
			package := CodePackage packageOfClass: classToRefactor ifNone: nil.
			self assert: package notNil.
			package hasUnsavedChanges: false ].
	! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 5/28/2017 23:26:45'!
allClassCategoriesOfTestData

	^Array 
		with: self classCategoryOfTestData 
		with: self anotherClassCategoryOfTestData 
		with: self classCategoryOfTestDataToAvoid ! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 3/4/2019 14:59:42'!
anotherClassCategoryOfTestData

	^self classCategoryOfTestData,'-Another'! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 3/4/2019 14:59:50'!
classCategoryOfTestDataToAvoid

	^self classCategoryOfTestData,'-toAvoid'! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 8/15/2018 12:03:16'!
classToRefactorName

	^#ClassToRenameSelector! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 12/18/2019 20:32:41'!
createClassToRefactor

	classToRefactor := self createClassWithImplementorAndSenderNamed: self classToRefactorName subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 5/25/2017 23:14:02'!
createClassWithImplementorAndSenderInMetaTooNamed: aName subclassOf: aSuperclass categorizedAd: aCategory

	| newClass |
	
	newClass := self createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory.
	newClass class compile: oldSelector asString.
	newClass class compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass
! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 5/28/2017 21:49:40'!
createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory

	| newClass |
	
	newClass := self createClassNamed: aName subclassOf: aSuperclass category: aCategory.
	newClass compile: oldSelector asString.
	newClass compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass
! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'HAW 11/8/2018 15:30:15'!
assertWasNotRenamedInClass: aClass 

	| senderMethod |
	
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsOrRefersTo: newSelector).
	self assert: (senderMethod sendsOrRefersTo: oldSelector)! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'HAW 3/7/2020 18:44:08'!
assertWasRenamedInClass: aClass
	
	| senderMethod |
	
	self assert: (aClass includesSelector: newSelector).
	self deny: (aClass includesSelector: oldSelector).
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'HAW 3/7/2020 18:44:59'!
assertWasRenamedInClassAndMeta: aClass
	
	self
		assertWasRenamedInClass: aClass;
		assertWasRenamedInClass: aClass class! !

!RenameSelectorTest methodsFor: 'setup' stamp: 'HAW 5/28/2017 21:46:55'!
setUp

	super setUp.
	
	oldSelector := 'oldXyz__' asSymbol.
	newSelector := 'newXyz__' asSymbol.
	senderOfOldSelector := 'm1__' asSymbol.! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 12:03:44'!
test01MessageWithNoSendersCreatesNewMethodAndRemovesOldOne
	
	| rename |
	
	classToRefactor := self createClassNamed: self classToRefactorName..
	classToRefactor compile: oldSelector asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:30:40'!
test02SendersOfMessageAreRenamed
	
	| rename senderMethod |
	
	self createClassToRefactor.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:30:51'!
test03OtherMessagesSendsAreNotRenamed

	|  rename senderMethod selectorToKeep |

	selectorToKeep := #toKeep.
	self createClassToRefactor.
	classToRefactor compile: senderOfOldSelector asString , ' self ' , oldSelector asString , '. self ' , selectorToKeep asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector).
	self assert: (senderMethod sendsOrRefersTo: selectorToKeep) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test04OldSelectorCanNotBeEmpty

	self 
		assertCreation: [ RenameSelector from: '' asSymbol to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector oldSelectorCanNotBeEmptyErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test05NewSelectorCanNotBeEmpty

	self 
		assertCreation: [ RenameSelector from: oldSelector to: '' asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorCanNotBeEmptyErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/30/2019 18:01:00'!
test06CanRenameWithoutImplementors

	self shouldntFail: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/18/2018 15:48:43'!
test07AllImplementorsMustImplementOldSelector

	| invalidImplementors |
	
	invalidImplementors := Array with: Object>>#printString.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: invalidImplementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForInvalidImplementors: invalidImplementors ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test08AllSendersShouldSendOldSelector

	| implementors invalidSenders |
	
	oldSelector := #printString.
	implementors := Array with: Object>>oldSelector.
	invalidSenders := Array with: Object>>#size.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: invalidSenders ]
		failsWith: [ RenameSelector errorMessageForInvalidSenders: invalidSenders of: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test09NewSelectorMustBeOfSameTypeAsOldSelector

	oldSelector := #size.
	newSelector := #+.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test10NewSelectorMustHaveSameNumberOfArgumentsAsOldSelector

	oldSelector := #printOn:.
	newSelector := #do:separatedBy:.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector doesNotHaveSameNumberOfArgumentsAs: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test11NewImplementorsKeepMethodCategory
	
	| rename oldSelectorCategory |
	
	oldSelectorCategory := 'oldSelectorCategory'.
	self createClassToRefactor.
	classToRefactor organization classify: oldSelector under: oldSelectorCategory.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	self assert: oldSelectorCategory equals: (classToRefactor organization categoryOfElement: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:30:58'!
test12RenamesRecursiveMethodsWhenNotInSenders

	| rename senderMethod |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:31:04'!
test13RenamesRecursiveMethodsWhenInSenders

	| rename senderMethod implementorsAndSenders |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
	implementorsAndSenders := Array with: classToRefactor>>oldSelector.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: implementorsAndSenders senders: implementorsAndSenders.
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/8/2018 15:27:09'!
test14RenamesSymbolsEqualToOldSelector

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' #' , oldSelector asString, ' size'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:34:01'!
test15ScopeToRenameCanBeClassOnly
	
	| rename anotherClassSendingMessage |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector in: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage ! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:07:15'!
test16ScopeToRenameCanBeHierarchyOnly
	
	| rename anotherClassSendingMessage superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: classToRefactor categorizedAd: self classCategoryOfTestData.

	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage ! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:07:32'!
test17ScopeToRenameCanBeCategoryOnly
	
	| rename anotherClassSendingMessage  anotherClassToRefactor |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector category: self classCategoryOfTestDataToAvoid.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector inCategoryOf: classToRefactor organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage
		! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:07:49'!
test18ScopeToRenameCanBeCategoriesAndHierarchyOnly
	
	| rename anotherClassSendingMessage superclassToRefactor subclassToRefactor anotherClassToRefactor classInOtherCategoryToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self anotherClassCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: classToRefactor categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #AnotheclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	classInOtherCategoryToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #OtherCatClassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self anotherClassCategoryOfTestData.
	
	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector category: self classCategoryOfTestDataToAvoid.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector inCategoriesAndHierarchyOf: classToRefactor organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasRenamedInClassAndMeta: classInOtherCategoryToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage ! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:08:00'!
test19ScopeToRenameCanBeTheCompleteSystem
	
	| rename anotherClassToRefactor |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inSystem: Smalltalk.
	"This is just to be sure that no more than the expected methods will be renamed because I'm using Smalltalk as system.
	I could mock Smalltalk but I want to have a real test using Smalltalk, not a mock, that it is why I have these assertions here - Hernan"
	self assert: 4 equals: rename implementorsSize.
	self assert: 4 equals: rename sendersSize.
	
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor  
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test20CanNotRenameToItSelf

	self 
		assertCreation: [ RenameSelector from: oldSelector to: oldSelector asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorEqualToOldSelectorErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test21NewSelectorCanNotBeImplementedOnAnyClassImplementingOldSelector

	| implementors |
	
	oldSelector := #printString.
	newSelector := #size.
	implementors := Array with: Object>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector implementedInAll: (Array with: Object) ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 8/15/2018 11:52:37'!
test22WarnsWhenOverridesSuperclassImplementation

	| implementors |
	
	oldSelector := #negated.
	newSelector := #size.
	implementors := Array with: Number>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		warnsWith: [ RenameSelector warningMessageForImplementationOf: newSelector in: Number willOverrideImplementationIn: Object ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/29/2018 14:14:49'!
test23OldSelectorIsRenamedWhenDefinedInLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/29/2018 15:20:23'!
test24OldSelectorIsRenamedWhenDefinedInLiteralArrayMoreThanOnce

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/29/2018 15:22:15'!
test25OldSelectorIsRenamedWhenDefinedInMoreThanOneLiteralArrayMoreThanOnce

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'), #(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/29/2018 15:24:47'!
test26DoesNotRenameLiteralsThatBeginsWithOldSelectorInsideLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'1)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector). 
	self assert: (senderMethod sendsOrRefersTo: (oldSelector,'1') asSymbol) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 11/30/2018 15:04:14'!
test27DoesNotRenameLiteralsThatBeginsWithOldSelectorAndEndWithColonInsideLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,':)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector). 
	self assert: (senderMethod sendsOrRefersTo: (oldSelector,':') asSymbol) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/13/2018 18:46:23'!
test28CanRenameFromBinaryToKeywordOfOneParameter

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := '&&' asSymbol.
	newSelector := 'abc:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector). 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/13/2018 18:53:24'!
test29CanRenameFromKeywordOfOneParameterToBinary

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := 'abc:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsOrRefersTo: oldSelector).
	self assert: (senderMethod sendsOrRefersTo: newSelector). 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/13/2018 19:00:10'!
test30CanNotRenameFromBinaryToKeywordOfMoreThanOneParameter

	oldSelector := '&&' asSymbol.
	newSelector := 'abc:def:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/13/2018 18:59:39'!
test31CanNotRenameFromKeywordOfOneParameterToBinary

	oldSelector := 'abc:def:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'abc: p1 def: p2'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 20:08:36'!
test32HierarchyScopeRenamesSelectorsFromSuperclassDefiningSelector
	
	| rename superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: RefactoringClassTestData categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 3/7/2020 18:48:46'!
test33DoNotRenameSendersAndImplementorsWhoseCompilerClassIsDifferentToCompiler
	
	"Requested by Phil B. to avoid refactoring OMeta2 classes - Hernan"
	
	| rename anotherClassSendingMessage  |
	
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName 
		subclassOf: RefactoringClassTestData 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #ClassToAvoidRenameSelector 
		subclassOf: RefactoringClassTestData 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage class compile: 'compilerClass ^nil'.

	rename := RenameSelector 
		from: oldSelector 
		to: newSelector 
		inCategoryOf: classToRefactor 
		organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage.
	self assertWasRenamedInClass: anotherClassSendingMessage class.
	! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 6/25/2017 21:53:44'!
classToRefactorName

	^#ClassToRenameTemp! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 6/25/2017 21:53:44'!
methodNodeOf: aSourceCode

	^self methodNodeOf: aSourceCode in: self class! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 8/9/2018 23:23:12'!
methodNodeOf: aSourceCode in: aClass

	^aClass methodNodeFor: aSourceCode ! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 10/4/2017 17:04:33'!
test01VariableToRenameHasToBeDefined

	| newVariable oldVariable methodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1'.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ RenameTemporary errorMessageForTemporaryVariable: oldVariable notDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 2/25/2020 00:02:32'!
test02NewVariableNameCanNotBeEmpty

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := ' '.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 2/25/2020 00:06:33'!
test03NewVariableHasToBeValid

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := 'a b'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 2/25/2020 00:07:12'!
test04NewVariableNameCanNotBeDefinedInMethod

	| methodNode oldVariable |
	
	oldVariable := 'old'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: oldVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: oldVariable isAlreadyDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 5/24/2020 20:01:16'!
test05FailsIfNewTemporaryIsEqualToInstanceVariableInClass

	| methodNode oldVariable newVariable classToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 5/24/2020 20:01:16'!
test06FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass

	| methodNode oldVariable newVariable classToRefactor superclassToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	superclassToRefactor := self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: superclassToRefactor.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test07RenameCreatesNewTemporaryVariableAndDeletesOldOne

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable)! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 8/9/2018 19:28:42'!
test08RenameChangesReferencesFromOldVariableToNewVariable

	| methodNode oldVariable newVariable rename newMethodNode assigmentNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ', oldVariable, ' := 1 + ', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	assigmentNode := newMethodNode block statements first.
	self assert: newVariable equals: assigmentNode variable name.
	self assert: newVariable equals: assigmentNode value arguments first name.! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test09RenamesArguments

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1: ', oldVariable, ' ^', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	self assert: newVariable equals: newMethodNode block statements first expr name.! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 2/25/2020 00:07:53'!
test10NewVariableNameCanNotBeDefinedAsBlockArgument

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | [:', newVariable, ' | ^1 ]'.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test11RenamesBlockArguments

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [:', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first arguments first name.
	"asserts it renamed reference to block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:12:33'!
test12RenamesBlockTemporaries

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [ |', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first temporaries first name.
	"asserts it renamed reference to block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:15:28'!
test13OldNodeMustBePartOfMethodNode

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	oldVariableNode := methodNode tempNodes anyOne.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode copy to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeNotPartOfMethodNodeErrorDescription ]	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:15:28'!
test14OldVariableNodeMustBeArgOrTempNode

	| methodNode newVariable oldVariableNode |
	
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 self'.
	oldVariableNode := methodNode block statements first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeMustBeArgOrTempNodeErrorDescription ]	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 12/17/2019 19:15:28'!
test15RenameVariablesWithSameNameInDifferentBlocksCorrectly

	| methodNode oldVariable newVariable oldVariableNode newMethodNode rename |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',oldVariable,'| ^',oldVariable, '].[|',oldVariable,'| ^',oldVariable, '].'.
	oldVariableNode := methodNode block statements first temporaries first.
	
	rename := RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self 
		assert: 'm1 [|',newVariable,'| ^',newVariable, '].[|',oldVariable,'| ^',oldVariable, '].'
		equals: newMethodNode sourceText! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 2/25/2020 00:08:04'!
test16CantRenameVariableToANameAlreadyUsedInAnUpperBlock

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',newVariable,'| [|',oldVariable,'| ^', oldVariable,']. ^',newVariable,' ]'.
	oldVariableNode := methodNode block statements first statements first temporaries first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [
			NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]
	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 2/29/2020 18:09:43'!
test17RenamesOneCharVariableAfterAReturnWithoutCharsAfterThatCorrectly

	| methodNode oldVariable newVariable rename newSource |
	
	"This test is due to a fixed error in the parser. 
	See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat"
	
	oldVariable := 'o'.
	newVariable := 'n'.

	methodNode := self methodNodeOf: 'm1 |',oldVariable,'| ^',oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newSource := rename apply.
	
	self 
		assert: 'm1 |',newVariable,'| ^',newVariable
		equals: newSource
		! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 4/26/2020 15:19:20'!
test18CantRenameVariableToAReservedName

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	
	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		methodNode := self methodNodeOf: 'm1 |' , oldVariable , '| ^ ' , oldVariable.

		self 
			assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:43:08'!
test02ClassesWithNoReferencesAndNoSubclassesAreSafetelyRemoved

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove.
	safeRemove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:43:11'!
test03RemovingTheMetaclassRemovesTheClass

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove class.
	safeRemove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 16:40:06'!
test04CanNotRemoveClassWithReferencesOutsideHierarchy

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemove))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:43:21'!
test05CanRemoveClassWithReferencesFromItself

	| classToRemove methodNameReferencingClass remove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	methodNameReferencingClass := #m1.
	classToRemove compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	remove := SafelyRemoveClass of: classToRemove.
	remove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:43:27'!
test06WarnIfClassToRemoveHasSubclasses

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self 
		assertCreation: [ SafelyRemoveClass of: classToRemove ]
		warnsWith: [ SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: (Array with: classToRemoveSubclass) ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/17/2018 16:38:44'!
test07CanNotRemoveClassWhenSubclassesHaveReferencesOutsideTheHierarchy

	| classToRemove classToRemoveSubclass classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemoveSubclass.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemoveSubclass name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemoveSubclass))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]	
! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:47:12'!
test08HierarchyIsRemovedIfSubclassesWarningIsResumed

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.
	self assert: classToRemoveSubclass isObsolete
		
	
		! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 8/1/2018 16:47:17'!
test09CanRemoveIfReferencesToSubclassesAreInTheHierarchy

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classToRemove compile: 'm1 ^', classToRemoveSubclass name asString.
	classToRemoveSubclass compile: 'm2 ^', classToRemove name asString.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.	
	self assert: classToRemoveSubclass isObsolete.! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 12/18/2019 16:04:18'!
test10WarnWhenHasReferencesToName

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^#', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self referencesRefactoringWarning 
		withExceptionDo: [ :aWarning | | reference |
			self 
				assert: (SafelyRemoveClass warningMessageForReferencesToNames: (Array with: classToRemove))
				equals: aWarning messageText.
			self assert: 1 equals: aWarning numberOfReferences.
			reference := aWarning anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]! !

!SafelyRemoveClassTest methodsFor: 'test support' stamp: 'HAW 8/1/2018 15:48:56'!
assertSubclassesReturnsACopy

	"This is a precondition for the remove to work properly with subclasses of the class to remove. 
	I do not put it in the refactoring to avoid innecesary checks and because tests verify preconditions. 
	It is not a seprate test because I do not want senders of this precondition to run if it fails - Hernan"
	self deny: self class superclass subclasses == self class superclass subclasses! !

!SafelyRemoveClassTest methodsFor: 'test support' stamp: 'HAW 8/1/2018 16:47:22'!
safelyRemoveHierarchyOf: classToRemove
		
	| remove |
	
	"see comment of #assertSubclassesReturnsACopy - Hernan"
	self assertSubclassesReturnsACopy.
	
	[ remove := SafelyRemoveClass of: classToRemove ]
		on: self refactoringWarning 
		do: [ :aWarning | 
			self assert: (SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: classToRemove allSubclasses) equals: aWarning messageText.
			aWarning resume ].
		
	remove apply.! !

!SafelyRemoveClassTest methodsFor: 'class factory' stamp: 'HAW 7/11/2018 16:56:05'!
classToRemoveName

	"I can not use the symbol directly because it would be a reference - Hernan"
	^ 'ClassToRemove' asSymbol.
	! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
methodSourceCodeNamed: aSelector withTemporaryVariableDeclaration: aTemporaryVariableName

	^aSelector, '| ', aTemporaryVariableName, ' |'.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:31:12'!
methodSourceCodeNamed: aSelector withTemporaryVariableDeclarations: temporaryVariableNames
	^String streamContents: [ :stream |
		stream nextPutAll: aSelector, '| '.
		temporaryVariableNames do: [ :temporaryVariableName |
			stream nextPutAll: temporaryVariableName, ' ' ].	
		stream nextPut: $|. ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
test01ApplyChangesTemporaryVariableToInstanceVariable

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: variableName).
	self assert: (classToRefactor canUnderstand: messageName).
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: 0 equals: changedMethodNode temporaries size.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
test02ApplyDoesntChangeRestOfSourceCode

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode restOfSourceCode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName. 
	restOfSourceCode _ variableName, ' := 2.
	^', variableName.
	sourceText _ sourceText, restOfSourceCode.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: (changedMethodNode sourceText findString: restOfSourceCode :: > 0).! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'HAW 5/19/2019 17:35:13'!
test03TemporaryToChangeShouldExist

	|  classToRefactor messageName methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	sourceText _ messageName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: 'a' fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable inexistentTemporaryErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
test04TemporaryShouldNotExistInOtherClassMethod

	|  classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	classToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: classToRefactor hasSourceEqualTo: secondSourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
test05InstanceVariableShouldNotExistInSubclass

	| classToRefactor messageName methodNode sourceText variableName subclassToRefactor |

	variableName _ 'a'.
	
	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: variableName
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	messageName _ #m1.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclaration: variableName.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.

	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsAsInstVarInSubclassesErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText.
			self assert: (subclassToRefactor instVarNames includes: variableName) ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:26:04'!
test06TemporaryShouldNotExistInSubclassMethod

	| classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText subclassToRefactor |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	subclassToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: subclassToRefactor hasSourceEqualTo: secondSourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/28/2019 19:13:24'!
test07RemovesPipesWhenRemovingLastTemporaryInMethod

	| variableName classToRefactor messageName refactoring sourceText methodNode |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1 '.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/25/2019 00:30:25'!
test08DoesntRemoveOtherVariables

	| variableName classToRefactor messageName refactoring sourceText methodNode otherVariableName |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	otherVariableName _ 'b'.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclarations: {variableName. otherVariableName}.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1|  b |'.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/28/2019 00:17:27'!
test09TemporaryShouldNotExistInOtherBlockInSameMethod

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	
	self 
		should: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText. ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 5/28/2019 19:52:12'!
test10RemovesPipesWhenRemovingLastTemporaryInBlock

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString newSourceCode refactoring |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	newSourceCode _ messageName, '
	[   ].'.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: newSourceCode.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 12/22/2019 18:18:26'!
test11TemporaryShouldNotExistAsArgumentInOtherBlocksInSameMethod

	| classToRefactor messageName methodNode sourceText variableName blockWithTempString blockWithArgumentString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithTempString _ '[ | ', variableName, ' | ].'.
	blockWithArgumentString _ '[ :', variableName, ' | ].'.
	sourceText _ messageName, ' ', blockWithTempString, ' ', blockWithArgumentString.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription ].
! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 12/22/2019 18:38:40'!
test12CantExtractArgument

	| classToRefactor methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	sourceText := 'm1: a'.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: #m1:
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: 'a' fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable inexistentTemporaryErrorDescription ].
! !

!TemporaryToInstanceVariableTest methodsFor: 'assertions' stamp: 'HAW 5/19/2019 17:29:33'!
assertMethodNamed: aMethodName in: aClass hasSourceEqualTo: aSourceCode

	| changedMethodNode |

	changedMethodNode _ self methodNodeIn: aClass named: aMethodName.
	self assert: aSourceCode equals: changedMethodNode sourceText.
			! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 5/19/2019 17:29:44'!
classToRefactorName

	^ #ClassToChangeVariable.! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 5/19/2019 17:31:47'!
compileMethodNodeIn: aClass named: aSelector sourceCode: sourceCode

	aClass compile: sourceCode.

	^self methodNodeIn: aClass named: aSelector.! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 5/19/2019 17:31:40'!
methodNodeIn: aClass named: aSelector

	^ aClass compiledMethodAt: aSelector :: methodNode.! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 11/1/2019 17:04:26'!
assertIsLogged: aString times: stringCount and: aBlock

	| logContent startPosition indexOfSubstring |

	logContent := self userChangesForTestsFile textContents.
 
	startPosition := 1.
	stringCount <= 0
		ifTrue: [ self deny: (logContent includesSubString: aString) ]
		ifFalse:[
			stringCount timesRepeat: [
				indexOfSubstring := logContent findString: aString startingAt: startPosition.
				self assert: indexOfSubstring > 0.
				startPosition := indexOfSubstring + 1 ].
			indexOfSubstring := logContent findString: aString startingAt: startPosition.
			self assert: 0 equals: indexOfSubstring].
		
	aBlock value: logContent value: startPosition.
	

	! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 11/1/2019 17:03:04'!
assertIsLoggedOnce: aString

	self assertIsLoggedOnce: aString and: [:logContents :nextPosition | ] ! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 11/1/2019 17:02:33'!
assertIsLoggedOnce: aString and: aBlock

	self assertIsLogged: aString times: 1 and: aBlock! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 11/1/2019 17:03:30'!
assertIsLoggedTwice: aString

	self assertIsLogged: aString times: 2 and: [:logContents :nextPosition | ] ! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 11/1/2019 17:07:50'!
changeUserChangesFileWhile: aBlock

	^ Preferences use: self userChangesForTestsFileExtension asUserChangesFileNameExtensionWhile: aBlock ! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 11/1/2019 17:07:57'!
scanChangesFromFile

	| fileStream changeList |

	fileStream := Smalltalk defaultUserChangesName asFileEntry readStream.
	changeList := ChangeList new scanFile: fileStream from: 0 to: fileStream size.

	^ changeList changeList.
! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 11/1/2019 17:08:00'!
userChangesForTestsFile

	^self userChangesForTestsFileName asFileEntry! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 11/1/2019 17:08:04'!
userChangesForTestsFileExtension

	^'.test.changes' ! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 11/1/2019 17:08:07'!
userChangesForTestsFileName

	^(FileIOAccessor default baseNameFor: Smalltalk imageName), self userChangesForTestsFileExtension! !

!ChangesTest methodsFor: 'setUp/tearDown' stamp: 'HAW 12/17/2019 10:36:27'!
tearDown

	self userChangesForTestsFile delete.
	super tearDown ! !

!ChangesTest methodsFor: 'test data' stamp: 'HAW 12/17/2019 10:37:01'!
createTestDataClass

	^ self createClassNamed: self testDataClassName 
		! !

!ChangesTest methodsFor: 'test data' stamp: 'HAW 11/1/2019 17:08:21'!
testDataClassName

	^ #LogChangesTestClass__! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 11/1/2019 17:08:27'!
test01ScanNewClassChange

	| newClass newClassChange |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClassChange := self scanChangesFromFile last.

		self assert: newClassChange changeType equals: #classDefinition.
		self assert: newClassChange changeClass equals: newClass.
		self deny: newClassChange stamp isNil.
		self deny: newClassChange isTestClassChange.
	] ! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 11/1/2019 17:08:31'!
test02ScanNewMethodChange

	| newMethodChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.

		newMethodChange := self scanChangesFromFile last.

		self assert: newMethodChange changeType equals: #method.
		self assert: newMethodChange methodSelector equals: #m1.
		self assert: newMethodChange changeClass equals: newClass.
		self assert: newMethodChange prior isNil.
		self deny: newMethodChange stamp isNil ] ! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 11/1/2019 17:08:34'!
test03ScanMethodModifiedChange

	| methodModifiedChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass compile: 'm1 ^ 2' classified: 'a-category'.

		methodModifiedChange := self scanChangesFromFile last.

		self assert: methodModifiedChange changeType equals: #method.
		self assert: methodModifiedChange methodSelector equals: #m1.
		self assert: methodModifiedChange changeClass equals: newClass.
		self deny: methodModifiedChange prior isNil.
		self deny: methodModifiedChange stamp isNil ]
! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 11/1/2019 17:08:37'!
test04ScanMethodRemovalChange

	| methodRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		methodRemovalChange := self scanChangesFromFile last.

		self assert: methodRemovalChange isMethodDeletion.
		self assert: methodRemovalChange methodSelector equals: #m1.
		self deny: methodRemovalChange stamp isNil ]
! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 11/1/2019 17:08:40'!
test05ScanClassRemovalChange

	| classRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass removeFromSystem.

		classRemovalChange := self scanChangesFromFile last.

		self assert: classRemovalChange changeType equals: #classRemoval.
		self assert: classRemovalChange changeClassName equals: self testDataClassName.
		self assert: classRemovalChange isClassDeletion.
		self deny: classRemovalChange stamp isNil ]

! !

!ChangesTest methodsFor: 'scan tests' stamp: 'MGD 12/19/2019 19:37:24'!
test06ScanClassRenamedChange

	| classRenamedChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		classRenamedChange := self scanChangesFromFile last.

		self assert: classRenamedChange changeType equals: #classRenamed.
		self assert: classRenamedChange changeClassName equals: self testDataClassName.
		self assert: classRenamedChange newClassName equals: #NewTestClass__.
		self deny: classRenamedChange stamp isNil. ]
! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 10/5/2020 11:33:10'!
test01AddingAClassShouldBeLogged
	" Log format expected:

	!!classDefinition: #ClassName category: 'Category' stamp: changeStamp!!
	Object subclass: #ClassName
		instanceVariableNames: '...'
		classVariableNames: '...'
		poolDictionaries: '....'
		category: 'Category'!!
	"
	| newClass |
	
	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.

		self assertIsLoggedOnce: '!!classDefinition: #', self testDataClassName, ' category: ''', self classCategoryOfTestData,''' stamp:'.
		self assertIsLoggedOnce: newClass definition ] ! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 11/1/2019 17:08:48'!
test02AddingANewMethodShouldBeLogged
	" Log format expected:

	!!Class methodsFor: 'category' stamp: 'author stamp'!!
	methodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.

		self assertIsLoggedOnce: '!!', self testDataClassName, ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'm1 ^ 1'] 
! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 11/1/2019 17:08:50'!
test03ChangingAMethodShouldBeLogged
	" Log format expected:

	!!Class methodsFor: 'category' stamp: 'author stamp'!!
	newMethodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.
		aClass compile: 'm1 ^ 2' classified: 'a-category'.

		self assertIsLoggedTwice: '!!', self testDataClassName , ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'prior: '.
		self assertIsLoggedOnce: 'm1 ^ 2' ] ! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 11/1/2019 17:08:53'!
test04RemovingAMethodShouldBeLogged
	" Log format expected:

	!!methodRemoval: #ClassName selector stamp: changeStamp!!
	methodSourceCode
	"
	| newClass methodSource readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		self 
			assertIsLoggedOnce: '!!methodRemoval: ',self testDataClassName, ' #m1 stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				methodSource := readStream nextLine.
		
				self assert: 'm1 ^ 1!!' equals: methodSource ]]! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 11/1/2019 17:08:56'!
test05RemovingAClassShouldBeLogged
	" Log format expected:

	!!classRemoval: #ClassName stamp: changeStamp!!
	classDefinitionMessage
	"
	| newClass definition loggedDefinition readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		definition := newClass definition.
		newClass removeFromSystem.

		self 
			assertIsLoggedOnce: '!!classRemoval: #', self testDataClassName, ' stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				loggedDefinition := readStream upToEnd.
				
				self assert: definition,'!!' equals: loggedDefinition ]]! !

!ChangesTest methodsFor: 'log tests' stamp: 'MGD 3/30/2020 17:00:58'!
test06RenamingAClassShouldBeLogged
	" Log format expected:

	!!classRenamed: #OldName as: #NewName stamp  stamp:!!
	Smalltalk renameClassNamed: #OldName as: #NewName
	"
	| newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		self assertIsLoggedOnce: '!!classRenamed: #', self testDataClassName, ' as: #NewTestClass__ stamp:'.
		self assertIsLoggedOnce: 'Smalltalk renameClassNamed: #', self testDataClassName, ' as: #NewTestClass__'.
	]! !

!WriteStreamTest methodsFor: 'tests' stamp: 'jpb 8/2/2019 23:49:13'!
testIsStream
	self assert: ((WriteStream on: '') is: #Stream).! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:27:06'!
testNewLineTabWhenConditionIsFalseDoesNotPutANewLineTabInStream

	| stream |

	stream := WriteStream on: ''.
	stream newLineTab: 2 when: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:28:04'!
testNewLineTabWhenConditionIsTruePutsANewLineTabInStream

	| stream |

	stream := WriteStream on: ''.
	stream newLineTab: 2 when: true.
	
	self assert: (String streamContents: [ :s | s newLineTab: 2 ]) equals: stream contents 
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:20:47'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	| stream |

	stream := WriteStream on: ''.
	stream newLineWhen: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:22:05'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	| stream |

	stream := WriteStream on: ''.
	stream newLineWhen: true.
	
	self assert: Character newLineCharacter asString equals: stream contents 
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:06:13'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	| stream |

	stream := WriteStream on: ''.
	stream nextPutAll: 'other' when: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:06:19'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	| stream |
	
	stream := WriteStream on: ''.
	stream nextPutAll: 'other' when: true.
	
	self assert: 'other' equals: stream contents
	
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:06:24'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	| stream |
	
	stream := WriteStream on: ''.
	stream nextPut: $t when: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:06:28'!
testNextPutWhenConditionIsTruePutsObjectInStream

	| stream |
	
	stream := WriteStream on: ''.
	stream nextPut: $t when: true.
	
	self assert: 't' equals: stream contents 
	
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:13:06'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	| stream |
	
	stream := WriteStream on: ''.
	stream print: Object when: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:06:44'!
testPrintWhenConditionIsTruePrintsObjectInStream

	| stream |
	
	stream := WriteStream on: ''.
	stream print: Object when: true.
	
	self assert: Object printString equals: stream contents! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:42:20'!
testSpaceManyTimesWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := WriteStream on: ''.
	stream space: 2 when: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:43:01'!
testSpaceManyTimesWhenConditionIsTruePutSpacesInStream

	| stream |

	stream := WriteStream on: ''.
	stream space: 2 when: true.
	
	self assert: '  ' equals: stream contents 
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:19:06'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := WriteStream on: ''.
	stream spaceWhen: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:19:50'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	| stream |

	stream := WriteStream on: ''.
	stream spaceWhen: true.
	
	self assert: ' ' equals: stream contents 
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:24:34'!
testTabManyTimesWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := WriteStream on: ''.
	stream tab: 2 when: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:25:46'!
testTabManyTimesWhenConditionIsTruePutsTabsInStream

	| stream |

	stream := WriteStream on: ''.
	stream tab: 2 when: true.
	
	self assert: (String streamContents: [ :s | s tab: 2 ]) equals: stream contents 
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:22:50'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := WriteStream on: ''.
	stream tabWhen: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:23:28'!
testTabWhenConditionIsTruePutsATabInStream

	| stream |

	stream := WriteStream on: ''.
	stream tabWhen: true.
	
	self assert: Character tab asString equals: stream contents 
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:29:18'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript newLineWhen: false ]
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:38:57'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	self assertTranscriptContentsAdded: '.' after: [ Transcript newLineWhen: true; nextPut: $. ]! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:09:29'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPutAll: 'other' when: false ]
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:13:47'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	self assertTranscriptContentsAdded: 'other' after: [ Transcript nextPutAll: 'other' when: true ]
	
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:11:58'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPut: $t when: false ]
! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:15:05'!
testNextPutWhenConditionIsTruePutsObjectInStream

	self assertTranscriptContentsAdded: 't' after: [ Transcript nextPut: $t when: true ]
	
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:12:44'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript print: Object when: false ]
! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:15:34'!
testPrintWhenConditionIsTruePrintsObjectInStream

	self assertTranscriptContentsAdded: Object printString after: [ Transcript print: Object when: true ]
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:39:29'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript spaceWhen: false ]
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:40:09'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	self assertTranscriptContentsAdded: ' ' after: [ Transcript spaceWhen: true ]
	! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:40:49'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript tabWhen: false ]! !

!TranscriptTest methodsFor: 'tests' stamp: 'HAW 3/20/2019 07:41:35'!
testTabWhenConditionIsTruePutsATabInStream

	self assertTranscriptContentsAdded: Character tab asString after: [ Transcript tabWhen: true ]! !

!TranscriptTest methodsFor: 'assertions' stamp: 'HAW 3/20/2019 07:14:26'!
assertTranscriptContentsAdded: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	
	self assert: transcriptContentsBeforeBlockEvaluation, expectedAddedString equals: Transcript contents! !

!TranscriptTest methodsFor: 'assertions' stamp: 'HAW 3/20/2019 07:15:48'!
assertTranscriptContentsDoesNotChangeAfter: aBlock
	
	self assertTranscriptContentsAdded: '' after: aBlock! !

!RectangleTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:52:04'!
testIsRectangle
	self assert: (Rectangle new is: #Rectangle)! !

!ColorFormTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:30:35'!
testIsColorForm
	self assert: (ColorForm new is: #ColorForm).! !

!GrayFormTest methodsFor: 'as yet unclassified' stamp: 'jpb 8/2/2019 23:36:06'!
testIsGrayForm
	self assert: (GrayForm new is: #GrayForm)! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 10/23/2019 09:23:56'!
testErrorSignalsTheRightException

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self error: reason ]
		raise: Error
		withMessageText: reason ! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 10/23/2019 09:24:03'!
testShouldNotHappenBecauseSignalsTheRightError

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self shouldNotHappenBecause: reason ]
		raise: Error
		withMessageText: self shouldNotHappenBecauseErrorMessage, reason ! !

!ObjectTest methodsFor: 'error tests' stamp: 'HAW 10/23/2019 09:24:11'!
testShouldNotHappenSignalsTheRightError

	self 
		should: [ self shouldNotHappen ]
		raise: Error
		withMessageText: self shouldNotHappenErrorMessage ! !

!WeakMessageSendTest methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:44:58'!
testIsMessageSend
	self assert: (WeakMessageSend new is: #MessageSend).! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'HAW 10/23/2019 21:08:24'!
testActualClassIfAbsentEvaluatesBlockIfClassDoesNotExist

	self 
		assert: 1 
		equals: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) actualClassIfAbsent: [ 1])! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'HAW 10/23/2019 21:09:29'!
testActualClassIfAbsentReturnsClassIfClassExists

	self 
		assert: self class
		equals: ((MethodReference 
			class: self class  
			selector: thisContext selector) actualClassIfAbsent: [ self fail])! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'HAW 10/23/2019 21:05:22'!
testActualClassReturnNilWhenClassDoesNotExist

	self assert: (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol: #xx) actualClass isNil! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'HAW 10/23/2019 21:09:58'!
testActualClassReturnsClassIfExists

	self 
		assert: self class
		equals: (MethodReference 
			class: self class 
			selector: thisContext selector) actualClass ! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:42:10'!
testSourceCodeIfAbsentEvaluatesBlockIfClassDoesNotExist

	self 
		assert: 1
		equals: ((MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCodeIfAbsent: [ 1 ])! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:42:34'!
testSourceCodeIfAbsentEvaluatesBlockIfMethodDoesNotExist

	self 
		assert: 1
		equals: ((MethodReference class: self class selector:#xx) sourceCodeIfAbsent: [ 1 ])! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:43:23'!
testSourceCodeIfAbsentReturnsSourceCodeIfExists

	self 
		assert: (self class>>thisContext selector) sourceCode
		equals: ((MethodReference class: self class selector: thisContext selector) sourceCodeIfAbsent: [ self fail ])! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:11:15'!
testSourceCodeReturnsSourceCodeIfMethodExist

	self 
		assert: (self class compiledMethodAt: thisContext selector) sourceCode
		equals: (MethodReference 
			class: self class 
			selector: thisContext selector) sourceCode ! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:39:16'!
testSourceCodeSignalExceptionIfClassDoesNotExist

	self 
		should: [ (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCode ]
		raise: Error
		withMessageText: MethodReference classDoesNotExistErrorMessage ! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 10/23/2019 21:13:28'!
testSourceCodeSignalExceptionIfMethodDoesNotExist

	self 
		should: [ (MethodReference class: self class selector: #xx) sourceCode ]
		raise: Error
		withMessageText: Dictionary keyNotFoundErrorDescription ! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'HAW 1/1/2020 19:46:58'!
testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenActualClassIsNil

	self 
		assert: 1 
		equals: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'HAW 1/1/2020 19:47:05'!
testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenSelectorIsNotImplemented

	self 
		assert: 1 
		equals: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'HAW 1/1/2020 19:47:13'!
testCompiledMethodIfAbsentReturnsImplementedMethodWhenItExists

	self 
		assert: thisContext method 
		equals: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: thisContext selector) compiledMethodIfAbsent: [ self fail ])! !

!MethodReferenceTest methodsFor: 'test string version' stamp: 'HAW 4/9/2020 15:51:56'!
test01DefaultStringVersionIsClassAndSelector

	| stringVersion |
	
	stringVersion := (MethodReference method: thisContext method) stringVersion.
	
	self assert: self class name, ' ', thisContext selector equals: stringVersion! !

!MethodReferenceTest methodsFor: 'test string version' stamp: 'HAW 4/9/2020 15:53:25'!
test02CanPrefixStringVersion

	| methodReference prefix |
	
	prefix := 'a prefix'.
	methodReference := MethodReference method: thisContext method.
	methodReference prefixStringVersionWith: prefix.
	
	self assert: prefix,self class name, ' ', thisContext selector equals: methodReference stringVersion! !

!MethodReferenceTest methodsFor: 'test testing' stamp: 'HAW 1/2/2020 06:49:00'!
testReferencesParameterAtReturnsFalseWhenInvalid

	self deny: ((MethodReference 
		classSymbol: #X_Y_Z 
		classIsMeta: false 
		methodSymbol: #xx) referencesParameterAt: 1)! !

!MethodReferenceTest methodsFor: 'test testing' stamp: 'HAW 1/2/2020 06:49:03'!
testReferencesParameterAtReturnsTrueWhenTheParameterIsReferenced

	self assert: ((MethodReference 
		classSymbol: #MethodReference 
		classIsMeta: false 
		methodSymbol: #referencesParameterAt:) referencesParameterAt: 1)! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 19:59:47'!
testIsNotValidBinarySelectorIfItContainsACharacterNotAllowed

	self deny: #'(' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 20:01:07'!
testIsNotValidKeywordSelectorIfItContainsSeparators

	self deny: #'between: and:' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 19:52:35'!
testIsNotValidUnarySelectorIfItContainsCharactersUsedInBinarySelectors

	self deny: #'a+b' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 19:51:29'!
testIsNotValidUnarySelectorIfItStartsWithALetterAndContainsSeparators

	self deny: #'with spaces' isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 19:53:43'!
testIsValidBinarySelectorIfItContainsAnAllowedCharacterOrSequenceOfCharacters

	self
		assert: #+ isValidSelector;
		assert: #+-+ isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 20:00:35'!
testIsValidKeywordSelectorIfItContainsKeywordsWithColons

	self
		assert: #includes: isValidSelector;
		assert: #between:and: isValidSelector! !

!SymbolTest methodsFor: 'tests - selector validation' stamp: 'RNG 3/29/2020 19:50:31'!
testIsValidUnarySelectorIfItStartsWithALetterAndDoesNotContainSeparators

	self assert: #t234 isValidSelector! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:16:37'!
test00MessagesForTestingAreImplemented

	self assert: (self respondsTo: #_zz).
	self assert: (self respondsTo: #!!).
	! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:56:57'!
test01TypeOfMessageSentToLiteralNumberAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '1'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:58:55'!
test02TypeOfMessageSentToLiteralStringAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '''a'''! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:58:39'!
test03TypeOfMessageSentToLiteralSymbolAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '#a'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:12'!
test04TypeOfMessageSentToLiteralArrayAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '#(1)'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:23'!
test05TypeOfMessageSentToLiteralBraceArrayAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '{1}'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:33'!
test06TypeOfMessageSentToLiteralBlockAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '[1]'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:42'!
test07TypeOfMessageSentToLiteralCharacterAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '$a'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:51'!
test08TypeOfMessageSentToLiteralNilAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'nil'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:59:59'!
test09TypeOfMessageSentToLiteralTrueAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'true'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:00:08'!
test10TypeOfMessageSentToLiteralFalseAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'false'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:23:39'!
test11TypeOfMessageSentToSelfAreObtainedCorrectly

	self 
		assertTypeOfMessageSend: 'm1 self _zz'
		is: #undefinedUnary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
	self 
		assertTypeOfMessageSend: 'm1 self size'
		is: #unary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	self 
		assertTypeOfMessageSend: 'm1 self !! 2'
		is: #undefinedBinary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ].
	self 
		assertTypeOfMessageSend: 'm1 self = 2'
		is: #binary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ].! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:00:26'!
test12TypeOfMessageSentToSuperAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'super'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 11:50:52'!
test13TypesIsUndefinedForAnyMessageSendToSuperFromProtoObject

	self 
		assertTypeOfMessageSend: 'm1 super size'
		is: #undefinedUnary
		in: ProtoObject
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	self 
		assertTypeOfMessageSend: 'm1 super = 2'
		is: #undefinedBinary
		in: ProtoObject
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ].! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:00:37'!
test14TypeOfMessageSentToGlobalVarAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'Smalltalk'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 10/5/2020 12:24:04'!
test15TypeOfMessageSentToClassVarAreObtainedCorrectly

	ClassVar1 := 1.
	self assertTypesAreValidWhenMessageSendTo: 'ClassVar1'! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 10/5/2020 12:02:43'!
assertTypeOfBinaryMessageSend: aSourceCode is: aExpectedType

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]
! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 10/5/2020 12:02:47'!
assertTypeOfMessageSend: aSourceCode 
	is: aExpectedType
	in: aClassOrMetaClass 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	| parser ranges st80RangeType type |
	
	parser := SHParserST80 new.
	ranges := parser
		workspace: nil;
		classOrMetaClass: aClassOrMetaClass ;
		source: aSourceCode;
		allSource: aSourceCode;
		parse;
		ranges.
		
	st80RangeType := SHST80RangeType for: aSourceCode in: aClassOrMetaClass.
	st80RangeType lastRange: (aReceiverRangeBlock value: ranges).
	type := st80RangeType ofCurrentRangeOrMessageSendIn: (aMessageRangeBlock value: ranges).
	
	self assert: aExpectedType equals: type
	! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 10/5/2020 12:23:51'!
assertTypeOfMessageSend: aSourceCode 
	is: aExpectedType 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType
		in: self class
		receiverRange: aReceiverRangeBlock 
		messageRange: aMessageRangeBlock! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 10/5/2020 12:02:52'!
assertTypeOfUnaryMessageSend: aSourceCode is: aExpectedType

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ]
! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 10/5/2020 12:02:56'!
assertTypesAreValidWhenMessageSendTo: aReceiverAsString

	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' _zz' is: #undefinedUnary.
	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' size' is: #unary.

	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' !! 2' is: #undefinedBinary.
	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' = 2' is: #binary
	! !

!SHST80RangeTypeTest methodsFor: 'messages for testing' stamp: 'HAW 10/5/2020 12:15:40'!
!! something

	"Do not remove this method because it exists for its name to have implementors - Hernan"! !

!SHST80RangeTypeTest methodsFor: 'messages for testing' stamp: 'HAW 10/5/2020 12:15:50'!
_zz

	"Do not remove this method because it exists for its name to have implementors - Hernan"! !

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 1/11/2019 12:21:36'!
initialize
	a _ 1.
	b _ 2.! !

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 1/29/2019 10:59:12'!
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

!BecomeTestExperiment methodsFor: 'as yet unclassified' stamp: 'jmv 1/29/2019 10:59:16'!
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

!BecomeTestExperiment class methodsFor: 'as yet unclassified' stamp: 'jmv 3/8/2019 11:37:17'!
threeVars

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'c a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'BaseImageTests-Kernel'! !

!BecomeTestExperiment class methodsFor: 'as yet unclassified' stamp: 'jmv 3/8/2019 11:37:22'!
twoVars

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'BaseImageTests-Kernel'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'dtl 6/1/2004 21:53'!
basicANSISignaledExceptionTestSelectors

	^#( simpleIsNestedTest simpleOuterTest doubleOuterTest doubleOuterPassTest doublePassOuterTest simplePassTest simpleResignalAsTest simpleResumeTest simpleRetryTest simpleRetryUsingTest simpleReturnTest)! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:54'!
basicTestSelectors
	^ #(#simpleEnsureTest #simpleEnsureTestWithNotification #simpleEnsureTestWithUparrow #simpleEnsureTestWithError #signalFromHandlerActionTest #resumableFallOffTheEndHandler #nonResumableFallOffTheEndHandler #doubleResumeTest #simpleTimeoutWithZeroDurationTest #simpleTimeoutTest simpleNoTimeoutTest)! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:14'!
doSomethingElseString

	^'Do something else.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:14'!
doSomethingExceptionalString

	^'Do something exceptional.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:13'!
doSomethingString

	^'Do something.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:15'!
doYetAnotherThingString

	^'Do yet another thing.'! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:15'!
iterationsBeforeTimeout

	^ iterationsBeforeTimeout! !

!ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:16'!
iterationsBeforeTimeout: anInteger

	iterationsBeforeTimeout := anInteger! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/7/1999 15:03'!
log

	log == nil
		ifTrue: [log := OrderedCollection new].
	^log! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:30'!
suiteLog

	suiteLog == nil
		ifTrue: [suiteLog := OrderedCollection new].
	^suiteLog! !

!ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:15'!
testString

	^'This is only a test.'! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/8/1999 09:17'!
clearLog

	log := nil! !

!ExceptionTester methodsFor: 'logging' stamp: 'jmv 3/13/2012 12:33'!
contents

	^( self log
		inject: (WriteStream on: (String new: 80))
		into: [ :result :item |
			result 
				newLine; 
				nextPutAll: item;
				yourself] ) contents! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/7/1999 15:03'!
log: aString

	self log add: aString! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/12/1999 23:07'!
logTest: aSelector

	self suiteLog add: aSelector! !

!ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/8/1999 09:38'!
logTestResult: aString

	| index |
	index := self suiteLog size.
	self suiteLog 
		at: index
		put: ((self suiteLog at: index), ' ', aString)! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:13'!
doSomething

	self log: self doSomethingString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:14'!
doSomethingElse

	self log: self doSomethingElseString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:14'!
doSomethingExceptional

	self log: self doSomethingExceptionalString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:15'!
doYetAnotherThing

	self log: self doYetAnotherThingString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:16'!
methodWithError

	MyTestError signal: self testString! !

!ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:16'!
methodWithNotification

	MyTestNotification signal: self testString! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:51'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:49'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:52'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:27'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tpr 5/27/2004 21:50'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 00:37'!
simplePassTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex pass "expecting handler in #runTest:"]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 02:12'!
simpleResignalAsTest
	"ExceptionTester new simpleResignalAsTest"

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do:
			[:ex | ex resignalAs: MyTestError new]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'nice 12/27/2009 21:06'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:02'!
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

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:03'!
simpleRetryUsingTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex | ex retryUsing: [self doYetAnotherThing]]! !

!ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 00:59'!
simpleReturnTest

	| it |
	it :=
		[self doSomething.
		MyTestError signal.
		self doSomethingElse]
			on: MyTestError
			do: [:ex | ex return: 3].
	it = 3 ifTrue: [self doYetAnotherThing]! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 6/1/2004 21:56'!
doubleOuterPassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 6/1/2004 21:56'!
doublePassOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:09'!
simpleIsNestedTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:10'!
simpleOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:10'!
simplePassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:11'!
simpleResignalAsTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'RAA 12/8/2000 12:59'!
simpleResumeTestResults

	"see if we can resume twice"

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:23'!
simpleRetryTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingString;
			add: self doSomethingElseString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:23'!
simpleRetryUsingTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			yourself! !

!ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 02:22'!
simpleReturnTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 11/14/1999 17:26'!
doubleResumeTest

       [self doSomething.
       MyResumableTestError signal.
       self doSomethingElse.
       MyResumableTestError signal.
       self doYetAnotherThing]
               on: MyResumableTestError
               do: [:ex | ex resume].! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/7/1999 13:43'!
nonResumableFallOffTheEndHandler
	
	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/9/1999 16:07'!
resumableFallOffTheEndHandler

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 8/19/1999 01:39'!
signalFromHandlerActionTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[self doYetAnotherThing.
			MyTestError signal]! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 09:44'!
simpleEnsureTest

	[self doSomething.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 12:50'!
simpleEnsureTestWithError

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 10:15'!
simpleEnsureTestWithNotification

	[self doSomething.
	self methodWithNotification.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/9/1999 16:04'!
simpleEnsureTestWithUparrow

	[self doSomething.
	true ifTrue: [^nil].
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'jmv 5/7/2012 21:38'!
simpleNoTimeoutTest

	[ self doSomething ]
		valueWithin: 1 days onTimeout:
			[ self doSomethingElse ]! !

!ExceptionTester methodsFor: 'tests' stamp: 'brp 10/22/2004 12:00'!
simpleTimeoutTest

	| n |
	[1 to: 1000000 do: [ :i | n := i. self doSomething ] ]
		valueWithin: 50 milliSeconds onTimeout:
			[ self iterationsBeforeTimeout: n.
			self doSomethingElse ]! !

!ExceptionTester methodsFor: 'tests' stamp: 'brp 10/22/2004 12:00'!
simpleTimeoutWithZeroDurationTest

	[ self doSomething ]
		valueWithin: 0 seconds onTimeout:
			[ self doSomethingElse ].
	! !

!ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/7/1999 14:28'!
warningTest

	self log: 'About to signal warning.'.
	Warning signal: 'Ouch'.
	self log: 'Warning signal handled and resumed.'! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 11/14/1999 17:29'!
doubleResumeTestResults

       ^OrderedCollection new
               add: self doSomethingString;
               add: self doSomethingElseString;
               add: self doYetAnotherThingString;
               yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 09:21'!
nonResumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/19/1999 02:39'!
resumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 8/19/1999 01:51'!
signalFromHandlerActionTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 09:47'!
simpleEnsureTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 6/9/1999 17:44'!
simpleEnsureTestWithErrorResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 10:13'!
simpleEnsureTestWithNotificationResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 18:55'!
simpleEnsureTestWithUparrowResults

	^OrderedCollection new
		add: self doSomethingString;
"		add: self doSomethingElseString;"
		add: self doYetAnotherThingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 16:54'!
simpleNoTimeoutTestResults

	^OrderedCollection new
		add: self doSomethingString;
		yourself! !

!ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 17:44'!
simpleTimeoutTestResults

	| things |
	things := OrderedCollection new: self iterationsBeforeTimeout.

	self iterationsBeforeTimeout timesRepeat: [ things add: self  doSomethingString ].
	things add: self doSomethingElseString.

	^ things! !

!ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 16:52'!
simpleTimeoutWithZeroDurationTestResults

	^OrderedCollection new
		add: self doSomethingElseString;
		yourself! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/13/1999 01:25'!
runAllTests
	"ExceptionTester new runAllTests"

	self
		runBasicTests;
		runBasicANSISignaledExceptionTests! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/12/1999 23:54'!
runBasicANSISignaledExceptionTests

	self basicANSISignaledExceptionTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector]! !

!ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/9/1999 16:06'!
runBasicTests

	self basicTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector]! !

!ExceptionTester methodsFor: 'testing' stamp: 'brp 10/21/2004 17:40'!
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

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:54'!
primTestAdd: aNumber
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 14:01:17'!
primTestArcTan

	<primitive: 57>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:58'!
primTestDivideBy: aNumber
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:44'!
primTestEqual: aNumber
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 12:42:41'!
primTestExp
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 59>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 13:42:23'!
primTestExponent
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 53>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:41'!
primTestGreater: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:38'!
primTestGreaterEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:35'!
primTestLess: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:32'!
primTestLessEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 12:42:00'!
primTestLn
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 58>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:21:03'!
primTestMultiplyBy: aNumber
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:29'!
primTestNotEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^#fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 13:55:50'!
primTestSin
	<primitive: 56>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/28/2019 15:38:43'!
primTestSqrt
	<primitive: 55>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:21:01'!
primTestSubtract: aNumber
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ #fail! !

!BoxedFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/11/2019 17:05:25'!
primTestTruncated
	<primitive: 51>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:13'!
primTestAdd: aNumber
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 14:01:27'!
primTestArcTan

	<primitive: 557>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:25'!
primTestDivideBy: aNumber
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:38:02'!
primTestEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 12:42:31'!
primTestExp
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 559>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 13:42:28'!
primTestExponent
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 553>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:38:00'!
primTestGreater: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:57'!
primTestGreaterEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:54'!
primTestLess: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:52'!
primTestLessEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 12:42:10'!
primTestLn
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 558>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:40'!
primTestMultiplyBy: aNumber
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:37:49'!
primTestNotEqual: aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^#fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/14/2019 13:55:56'!
primTestSin
	<primitive: 556>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/28/2019 15:38:37'!
primTestSqrt
	<primitive: 555>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 2/25/2019 17:20:31'!
primTestSubtract: aNumber
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ #fail! !

!SmallFloat64 methodsFor: '*BaseImageTests-Kernel-Numbers' stamp: 'jmv 3/11/2019 17:05:16'!
primTestTruncated
	<primitive: 551>
	^ #fail! !

!Preferences class methodsFor: '*BaseImageTests' stamp: 'RNG 3/24/2020 22:44:27'!
withTemporaryValue: temporaryValue of: aPreferenceName do: aBlock
	"Allows to execute a piece of code with a temporary value of the given preference. Useful for testing purposes."
	
	| currentValue |
	currentValue := self perform: aPreferenceName.
	self setPreference: aPreferenceName toValue: temporaryValue.
	[ aBlock value ] ensure: [ self setPreference: aPreferenceName toValue: currentValue ]! !
