Class {
	#name : 'UnicodeNormalizationTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'tests' }
UnicodeNormalizationTest >> testCanonicalComposites [
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r00C5)) canonicalDecomposition asCodePoints asArray = #(16r0041 16r030A).
	self assert: (UnicodeString fromCodePoints: #(16r00C5)) canonicalComposition asCodePoints asArray = #(16r00C5).

	self assert: (UnicodeString fromCodePoints: #(16r00F4)) canonicalDecomposition asCodePoints asArray = #(16r006F 16r0302).
	self assert: (UnicodeString fromCodePoints: #(16r00F4)) canonicalComposition asCodePoints asArray = #(16r00F4).
]

{ #category : 'tests' }
UnicodeNormalizationTest >> testMultipleCombiningMarks [
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r1E69)) canonicalDecomposition asCodePoints asArray = #(16r0073 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r1E69)) canonicalComposition asCodePoints asArray = #(16r1E69).

	self assert: (UnicodeString fromCodePoints: #(16r1E0B 16r0323)) canonicalDecomposition asCodePoints asArray = #(16r0064 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r1E0B 16r0323)) canonicalComposition asCodePoints asArray = #(16r1E0D 16r0307).

	self assert: (UnicodeString fromCodePoints: #(16r0071 16r0307 16r0323)) canonicalDecomposition asCodePoints asArray = #(16r0071 16r0323 16r0307).
	self assert: (UnicodeString fromCodePoints: #(16r0071 16r0307 16r0323)) canonicalComposition asCodePoints asArray = #(16r0071 16r0323 16r0307).
]

{ #category : 'tests' }
UnicodeNormalizationTest >> testSingletons [
	"Examples from
	https://unicode.org/reports/tr15/#Norm_Forms
	"

	self assert: (UnicodeString fromCodePoints: #(16r212B)) canonicalDecomposition asCodePoints asArray = #(16r0041 16r030A).
	self assert: (UnicodeString fromCodePoints: #(16r212B)) canonicalComposition asCodePoints asArray = #(16r00C5).

	self assert: (UnicodeString fromCodePoints: #(16r2126)) canonicalDecomposition asCodePoints asArray = #(16r03A9).
	self assert: (UnicodeString fromCodePoints: #(16r2126)) canonicalComposition asCodePoints asArray = #(16r03A9).
]
