'From Cuis7.5 [latest update: #7608] on 23 September 2025 at 6:04:45 pm'!
'Description '!
!provides: 'TestDynamicCuisLibraries' 1 20!
!requires: 'DynamicCuisLibraries' 1 22 nil!
SystemOrganization addCategory: #TestDynamicCuisLibraries!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-Tests'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses01'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses02'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses03'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses04'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses05'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses06'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-Transient'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses07'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses08'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses09'!


!classDefinition: #Noclash_Float32ArrayCodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
FloatArray variableWordSubclass: #Noclash_Float32ArrayCodeModelExperiment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_Float32ArrayCodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_Float32ArrayCodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #Noclash_DclExample01 category: #'TestDynamicCuisLibraries-SubjectClasses01'!
Object subclass: #Noclash_DclExample01
	instanceVariableNames: 'a b c'
	classVariableNames: 'A B'
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses01'!
!classDefinition: 'Noclash_DclExample01 class' category: #'TestDynamicCuisLibraries-SubjectClasses01'!
Noclash_DclExample01 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #Noclash_DclExample02 category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Object subclass: #Noclash_DclExample02
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses02'!
!classDefinition: 'Noclash_DclExample02 class' category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Noclash_DclExample02 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #NoclashExisting_DclExample02 category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Object subclass: #NoclashExisting_DclExample02
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses02'!
!classDefinition: 'NoclashExisting_DclExample02 class' category: #'TestDynamicCuisLibraries-SubjectClasses02'!
NoclashExisting_DclExample02 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #NoclashAB_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashAB_DclExample03
	instanceVariableNames: 'a b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashAB_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashAB_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABD_DclExample03Subclass category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashAB_DclExample03 subclass: #NoclashABD_DclExample03Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABD_DclExample03Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABD_DclExample03Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashABC_DclExample03
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABC_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABC_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCD_DclExample03Subclass category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABC_DclExample03 subclass: #NoclashABCD_DclExample03Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABCD_DclExample03Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABCD_DclExample03Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCnoClassIvars_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashABCnoClassIvars_DclExample03
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABCnoClassIvars_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABCnoClassIvars_DclExample03 class
	instanceVariableNames: 'zz xx'!

!classDefinition: #NoclashACB_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashACB_DclExample03
	instanceVariableNames: 'a c b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashACB_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashACB_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample04 category: #'TestDynamicCuisLibraries-SubjectClasses04'!
Object subclass: #NoclashABC_DclExample04
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashABC_DclExample04 class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashABC_DclExample04 class
	instanceVariableNames: ''!

!classDefinition: #NoclashAC_DclExample04 category: #'TestDynamicCuisLibraries-SubjectClasses04'!
Object subclass: #NoclashAC_DclExample04
	instanceVariableNames: 'a c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashAC_DclExample04 class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashAC_DclExample04 class
	instanceVariableNames: ''!

!classDefinition: #NoclashB_DclExample04Sub category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashAC_DclExample04 subclass: #NoclashB_DclExample04Sub
	instanceVariableNames: 'b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashB_DclExample04Sub class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashB_DclExample04Sub class
	instanceVariableNames: ''!

!classDefinition: #Noclash_DclExample05Super category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object subclass: #Noclash_DclExample05Super
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'Noclash_DclExample05Super class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Noclash_DclExample05Super class
	instanceVariableNames: ''!

!classDefinition: #NoclashSub_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Noclash_DclExample05Super subclass: #NoclashSub_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashSub_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashSub_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashByte_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableByteSubclass: #NoclashByte_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashByte_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashByte_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashDoubleByte_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableDoubleByteSubclass: #NoclashDoubleByte_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashDoubleByte_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashDoubleByte_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashDoubleWord_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableDoubleWordSubclass: #NoclashDoubleWord_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashDoubleWord_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashDoubleWord_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashIndexed_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableSubclass: #NoclashIndexed_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashIndexed_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashIndexed_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashObject_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object subclass: #NoclashObject_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashObject_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashObject_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashWeak_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object weakSubclass: #NoclashWeak_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashWeak_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashWeak_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashWord_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableWordSubclass: #NoclashWord_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashWord_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashWord_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample06 category: #'TestDynamicCuisLibraries-SubjectClasses06'!
Object subclass: #NoclashABC_DclExample06
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses06'!
!classDefinition: 'NoclashABC_DclExample06 class' category: #'TestDynamicCuisLibraries-SubjectClasses06'!
NoclashABC_DclExample06 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCclasVarZZ_DclExample06 category: #'TestDynamicCuisLibraries-SubjectClasses06'!
Object subclass: #NoclashABCclasVarZZ_DclExample06
	instanceVariableNames: 'a b c'
	classVariableNames: 'ZZ'
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses06'!
!classDefinition: 'NoclashABCclasVarZZ_DclExample06 class' category: #'TestDynamicCuisLibraries-SubjectClasses06'!
NoclashABCclasVarZZ_DclExample06 class
	instanceVariableNames: ''!

!classDefinition: #NoclashAB_DclExample07 category: #'TestDynamicCuisLibraries-SubjectClasses07'!
Object subclass: #NoclashAB_DclExample07
	instanceVariableNames: 'z1 z2 z3 z4 z5 z6 z7 z8 z9 a b a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83 a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a121 a122 a123 a124 a125 a126 a127 a128 a129 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a140 a141 a142 a143 a144 a145 a146 a147 a148 a149 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a160 a161 a162 a163 a164 a165 a166 a167 a168 a169 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a180 a181 a182 a183 a184 a185 a186 a187 a188 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a200 a201 a202 a203 a204 a205 a206 a207 a208 a209 a210 a211 a212 a213 a214 a215 a216 a217 a218 a219 a220 a221 a222 a223 a224 a225 a226 a227 a228 a229 a230 a231 a232 a233 a234 a235 a236 a237 a238 a239 a240'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses07'!
!classDefinition: 'NoclashAB_DclExample07 class' category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashAB_DclExample07 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABD_DclExample07Subclass category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashAB_DclExample07 subclass: #NoclashABD_DclExample07Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses07'!
!classDefinition: 'NoclashABD_DclExample07Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashABD_DclExample07Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample07 category: #'TestDynamicCuisLibraries-SubjectClasses07'!
Object subclass: #NoclashABC_DclExample07
	instanceVariableNames: 'z1 z2 z3 z4 z5 z6 z7 z8 z9 a b a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83 a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a121 a122 a123 a124 a125 a126 a127 a128 a129 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a140 a141 a142 a143 a144 a145 a146 a147 a148 a149 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a160 a161 a162 a163 a164 a165 a166 a167 a168 a169 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a180 a181 a182 a183 a184 a185 a186 a187 a188 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a200 a201 a202 a203 a204 a205 a206 a207 a208 a209 a210 a211 a212 a213 a214 a215 a216 a217 a218 a219 a220 a221 a222 a223 a224 a225 a226 a227 a228 a229 a230 a231 a232 a233 a234 a235 a236 a237 a238 a239 a240 c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses07'!
!classDefinition: 'NoclashABC_DclExample07 class' category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashABC_DclExample07 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCD_DclExample07Subclass category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashABC_DclExample07 subclass: #NoclashABCD_DclExample07Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses07'!
!classDefinition: 'NoclashABCD_DclExample07Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses07'!
NoclashABCD_DclExample07Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashAB_DclExample08 category: #'TestDynamicCuisLibraries-SubjectClasses08'!
Object subclass: #NoclashAB_DclExample08
	instanceVariableNames: 'a b a1 a2 a3 a4 a5 a6 a7 a8 a9 a10'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses08'!
!classDefinition: 'NoclashAB_DclExample08 class' category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashAB_DclExample08 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABD_DclExample08Subclass category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashAB_DclExample08 subclass: #NoclashABD_DclExample08Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses08'!
!classDefinition: 'NoclashABD_DclExample08Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashABD_DclExample08Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample08 category: #'TestDynamicCuisLibraries-SubjectClasses08'!
Object subclass: #NoclashABC_DclExample08
	instanceVariableNames: 'a a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83 a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a121 a122 a123 a124 a125 a126 a127 a128 a129 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a140 a141 a142 a143 a144 a145 a146 a147 a148 a149 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a160 a161 a162 a163 a164 a165 a166 a167 a168 a169 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a180 a181 a182 a183 a184 a185 a186 a187 a188 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a200 a201 a202 a203 a204 a205 a206 a207 a208 a209 a210 a211 a212 a213 a214 a215 a216 a217 a218 a219 a220 a221 a222 a223 a224 a225 a226 a227 a228 a229 a230 a231 a232 a233 a234 a235 a236 a237 a238 a239 a240 b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses08'!
!classDefinition: 'NoclashABC_DclExample08 class' category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashABC_DclExample08 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCD_DclExample08Subclass category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashABC_DclExample08 subclass: #NoclashABCD_DclExample08Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses08'!
!classDefinition: 'NoclashABCD_DclExample08Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses08'!
NoclashABCD_DclExample08Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashAB_DclExample09 category: #'TestDynamicCuisLibraries-SubjectClasses09'!
Object subclass: #NoclashAB_DclExample09
	instanceVariableNames: 'a b a1 a2 a3 a4 a5 a6 a7 a8 a9 a10'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses09'!
!classDefinition: 'NoclashAB_DclExample09 class' category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashAB_DclExample09 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABD_DclExample09Subclass category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashAB_DclExample09 subclass: #NoclashABD_DclExample09Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses09'!
!classDefinition: 'NoclashABD_DclExample09Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashABD_DclExample09Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample09 category: #'TestDynamicCuisLibraries-SubjectClasses09'!
Object subclass: #NoclashABC_DclExample09
	instanceVariableNames: 'a a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83 a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a121 a122 a123 a124 a125 a126 a127 a128 a129 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a140 a141 a142 a143 a144 a145 a146 a147 a148 a149 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a160 a161 a162 a163 a164 a165 a166 a167 a168 a169 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a180 a181 a182 a183 a184 a185 a186 a187 a188 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a200 a201 a202 a203 a204 a205 a206 a207 a208 a209 a210 a211 a212 a213 a214 a215 a216 a217 a218 a219 a220 a221 a222 a223 a224 a225 a226 a227 a228 a229 a230 a231 a232 a233 a234 a235 a236 a237 a238 a239 a240 a241 a242 a243 a244 a245 a246 a247 a248 a249 a250 a251 a252 a253 a254 b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses09'!
!classDefinition: 'NoclashABC_DclExample09 class' category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashABC_DclExample09 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCD_DclExample09Subclass category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashABC_DclExample09 subclass: #NoclashABCD_DclExample09Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses09'!
!classDefinition: 'NoclashABCD_DclExample09Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses09'!
NoclashABCD_DclExample09Subclass class
	instanceVariableNames: ''!

!classDefinition: #Noclash_CodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Object subclass: #Noclash_CodeModelExperiment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_CodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_CodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #Noclash_PointCodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Object subclass: #Noclash_PointCodeModelExperiment
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_PointCodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_PointCodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #CodeModelTest1 category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #CodeModelTest1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'CodeModelTest1 class' category: #'TestDynamicCuisLibraries-Tests'!
CodeModelTest1 class
	instanceVariableNames: ''!

!classDefinition: #DclBasicTest category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #DclBasicTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'DclBasicTest class' category: #'TestDynamicCuisLibraries-Tests'!
DclBasicTest class
	instanceVariableNames: ''!

!classDefinition: #DclTest category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #DclTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'DclTest class' category: #'TestDynamicCuisLibraries-Tests'!
DclTest class
	instanceVariableNames: ''!

!classDefinition: #InstvarRemappingTest category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #InstvarRemappingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'InstvarRemappingTest class' category: #'TestDynamicCuisLibraries-Tests'!
InstvarRemappingTest class
	instanceVariableNames: ''!

!classDefinition: #NoclashProtoObject_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
ProtoObject subclass: #NoclashProtoObject_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashProtoObject_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashProtoObject_DclExample05 class
	instanceVariableNames: ''!


!Noclash_Float32ArrayCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:31'!
floatAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>! !

!Noclash_Float32ArrayCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:31'!
floatAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:39'!
a
	^a! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:42'!
b
	^b! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:45'!
c
	^c! !

!Noclash_DclExample01 methodsFor: 'initialization' stamp: 'jmv 8/2/2023 14:25:23'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:53:02'!
combined
	^ self a * 10 + self b * 10 + self c.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:37:41'!
maxVal
	^SmallInteger maxVal.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:39:07'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!Noclash_DclExample01 class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 14:25:08'!
new
	^ self basicNew initialize! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:45:30'!
combinedCIvars
	^aa *10 + bb! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 8/2/2023 14:44:20'!
processor
	^Processor! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:44:50'!
setClassIVars
	aa := 5.
	bb := 7.! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:45:23'!
a
	^a! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:45:23'!
b
	^b! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:54:12'!
c
	^c! !

!Noclash_DclExample02 methodsFor: 'initialization' stamp: 'jmv 8/2/2023 14:45:23'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:53:05'!
combined
	^ self a * 10 + self b * 10 + self c.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
maxVal
	^SmallInteger maxVal.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!Noclash_DclExample02 class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 14:45:23'!
new
	^ self basicNew initialize! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:50:43'!
combinedCIvars
	^aa *10 + bb! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
processor
	^Processor! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:50:39'!
setClassIVars
	aa := 5.
	bb := 7.! !

!NoclashExisting_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/7/2023 18:10:54'!
b
	^ b.! !

!NoclashExisting_DclExample02 methodsFor: 'initialization' stamp: 'jmv 8/7/2023 18:10:54'!
initialize
	a _ 1.
	b _ 2.
	c _ 3.! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 8/8/2023 15:52:02'!
combined
	^ 47! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 10/24/2023 12:23:50'!
maxVal
	^ SmallInteger maxVal-1.! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 10/24/2023 12:25:06'!
someLiteral
	^#(22 33)! !

!NoclashExisting_DclExample02 class methodsFor: 'instance creation' stamp: 'jmv 8/7/2023 18:10:54'!
new
	^ self basicNew initialize.! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:28:25'!
combinedCIvars
	^aa + bb! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:26:21'!
processor
	^ nil.! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:28:55'!
setClassIVars
	aa := 3.
	bb := 4.! !

!NoclashAB_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:11'!
b
	^ b.! !

!NoclashAB_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:11'!
initialize
	a := 7.
	b := 9.! !

!NoclashAB_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:12'!
combined
	 ^ a * 10 + b! !

!NoclashAB_DclExample03 methodsFor: 'other' stamp: 'jmv 8/11/2023 12:37:03'!
xk47
	^'xk47'! !

!NoclashAB_DclExample03 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:11'!
new
	^ self basicNew initialize.! !

!NoclashABD_DclExample03Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:40:26'!
d
	^d! !

!NoclashABD_DclExample03Subclass methodsFor: 'initialization' stamp: 'jmv 8/10/2023 11:26:59'!
initialize
	super initialize.
	d := 4.! !

!NoclashABD_DclExample03Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 10:48:38'!
combined
	 ^ a * 10 + b * 10 + d + super combined! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
a
	^a! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
b
	^b! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
c
	^c! !

!NoclashABC_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:22'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:03'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABC_DclExample03 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:22'!
new
	^ self basicNew initialize! !

!NoclashABCD_DclExample03Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:42:33'!
d
	^d! !

!NoclashABCD_DclExample03Subclass methodsFor: 'initialization' stamp: 'jmv 8/4/2023 16:50:20'!
initialize
	super initialize.
	d := 4.! !

!NoclashABCD_DclExample03Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 11:07:31'!
combined
	 ^ a * 10 + b * 10 + c * 10 + d + super combined! !

!NoclashACB_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/10/2023 12:05:25'!
initialize2
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:24'!
a
	^a! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:28'!
b
	^b! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:32'!
c
	^c! !

!NoclashABC_DclExample06 methodsFor: 'initialization' stamp: 'jmv 8/11/2023 10:50:51'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:11'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:14'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:17'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABCclasVarZZ_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:53:44'!
setAndAnswerZZ
	ZZ := 1274.
	^ZZ! !

!NoclashABCclasVarZZ_DclExample06 class methodsFor: 'other' stamp: 'jmv 8/11/2023 12:33:46'!
setAndAnswerZZ2
	ZZ := 12742.
	^ZZ! !

!NoclashAB_DclExample07 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:11'!
b
	^ b.! !

!NoclashAB_DclExample07 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:11'!
initialize
	a := 7.
	b := 9.! !

!NoclashAB_DclExample07 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:12'!
combined
	 ^ a * 10 + b! !

!NoclashAB_DclExample07 methodsFor: 'other' stamp: 'jmv 8/11/2023 12:37:03'!
xk47
	^'xk47'! !

!NoclashAB_DclExample07 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:11'!
new
	^ self basicNew initialize.! !

!NoclashABD_DclExample07Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:40:26'!
d
	^d! !

!NoclashABD_DclExample07Subclass methodsFor: 'initialization' stamp: 'jmv 8/10/2023 11:26:59'!
initialize
	super initialize.
	d := 4.! !

!NoclashABD_DclExample07Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 10:48:38'!
combined
	 ^ a * 10 + b * 10 + d + super combined! !

!NoclashABC_DclExample07 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
a
	^a! !

!NoclashABC_DclExample07 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
b
	^b! !

!NoclashABC_DclExample07 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
c
	^c! !

!NoclashABC_DclExample07 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:22'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample07 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:03'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample07 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample07 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABC_DclExample07 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:22'!
new
	^ self basicNew initialize! !

!NoclashABCD_DclExample07Subclass methodsFor: 'accessing' stamp: 'jmv 7/30/2025 12:40:47'!
d
	^d! !

!NoclashABCD_DclExample07Subclass methodsFor: 'initialization' stamp: 'jmv 7/30/2025 12:41:03'!
initialize
	super initialize.
	d := 4.! !

!NoclashABCD_DclExample07Subclass methodsFor: 'other' stamp: 'jmv 7/30/2025 12:41:14'!
combined
	 ^ a * 10 + b * 10 + c * 10 + d + super combined! !

!NoclashAB_DclExample08 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:11'!
b
	^ b.! !

!NoclashAB_DclExample08 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:11'!
initialize
	a := 7.
	b := 9.! !

!NoclashAB_DclExample08 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:12'!
combined
	 ^ a * 10 + b! !

!NoclashAB_DclExample08 methodsFor: 'other' stamp: 'jmv 8/11/2023 12:37:03'!
xk47
	^'xk47'! !

!NoclashAB_DclExample08 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:11'!
new
	^ self basicNew initialize.! !

!NoclashABD_DclExample08Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:40:26'!
d
	^d! !

!NoclashABD_DclExample08Subclass methodsFor: 'initialization' stamp: 'jmv 8/10/2023 11:26:59'!
initialize
	super initialize.
	d := 4.! !

!NoclashABD_DclExample08Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 10:48:38'!
combined
	 ^ a * 10 + b * 10 + d + super combined! !

!NoclashABC_DclExample08 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
a
	^a! !

!NoclashABC_DclExample08 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
b
	^b! !

!NoclashABC_DclExample08 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
c
	^c! !

!NoclashABC_DclExample08 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:22'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample08 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:03'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample08 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample08 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABC_DclExample08 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:22'!
new
	^ self basicNew initialize! !

!NoclashABCD_DclExample08Subclass methodsFor: 'accessing' stamp: 'jmv 7/30/2025 12:40:47'!
d
	^d! !

!NoclashABCD_DclExample08Subclass methodsFor: 'initialization' stamp: 'jmv 7/30/2025 12:41:03'!
initialize
	super initialize.
	d := 4.! !

!NoclashABCD_DclExample08Subclass methodsFor: 'other' stamp: 'jmv 7/30/2025 12:41:14'!
combined
	 ^ a * 10 + b * 10 + c * 10 + d + super combined! !

!NoclashAB_DclExample09 methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:08:35'!
b
	^ b.! !

!NoclashAB_DclExample09 methodsFor: 'initialization' stamp: 'jmv 9/22/2025 16:08:35'!
initialize
	a := 7.
	b := 9.! !

!NoclashAB_DclExample09 methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:35'!
combined
	 ^ a * 10 + b! !

!NoclashAB_DclExample09 methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:35'!
xk47
	^'xk47'! !

!NoclashAB_DclExample09 class methodsFor: 'instance creation' stamp: 'jmv 9/22/2025 16:08:35'!
new
	^ self basicNew initialize.! !

!NoclashABD_DclExample09Subclass methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:08:44'!
d
	^d! !

!NoclashABD_DclExample09Subclass methodsFor: 'initialization' stamp: 'jmv 9/22/2025 16:08:44'!
initialize
	super initialize.
	d := 4.! !

!NoclashABD_DclExample09Subclass methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:45'!
combined
	 ^ a * 10 + b * 10 + d + super combined! !

!NoclashABC_DclExample09 methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:08:53'!
a
	^a! !

!NoclashABC_DclExample09 methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:08:53'!
b
	^b! !

!NoclashABC_DclExample09 methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:08:53'!
c
	^c! !

!NoclashABC_DclExample09 methodsFor: 'initialization' stamp: 'jmv 9/22/2025 16:08:53'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample09 methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:53'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample09 methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:53'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample09 methodsFor: 'other' stamp: 'jmv 9/22/2025 16:08:53'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABC_DclExample09 class methodsFor: 'instance creation' stamp: 'jmv 9/22/2025 16:08:53'!
new
	^ self basicNew initialize! !

!NoclashABCD_DclExample09Subclass methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:52:17'!
bb
	^b! !

!NoclashABCD_DclExample09Subclass methodsFor: 'accessing' stamp: 'jmv 9/22/2025 16:09:01'!
d
	^d! !

!NoclashABCD_DclExample09Subclass methodsFor: 'initialization' stamp: 'jmv 9/22/2025 16:09:01'!
initialize
	super initialize.
	d := 4.! !

!NoclashABCD_DclExample09Subclass methodsFor: 'other' stamp: 'jmv 9/22/2025 16:09:01'!
combined
	 ^ a * 10 + b * 10 + c * 10 + d + super combined! !

!Noclash_CodeModelExperiment methodsFor: 'sample methods' stamp: 'jmv 8/2/2023 16:43:04'!
method01
	"Just for testing.
	"

	| a b1 b2 b3 b4 b5 c d |
	a := Smalltalk.  								"Method literal is a ReadOnlyVariableBinding"
	b1 := Point.  								"Method literal is a ReadOnlyVariableBinding"
	b2 := Point class.  								"Method literal is a ReadOnlyVariableBinding."
	b3 := Smalltalk at: #Point.  								"Additional method literal is the symbol #Point"
	b4 := `Smalltalk at: #Point`. 								"Method literal is class Point"
	b5 := `(Smalltalk at: #Point) class`. 								"Method literal is metaclass Point"
	c := Display.  								"Method literal is an Association"
	d := `Smalltalk at: #Display`.  								"Method literal is the instance of DisplayScreen"
	^ {a. b1. b2. b3. b4. b5. c. d}.
"Qué cosas pueden ser literal de un metodo?
+ isLiteral == true (Booleans, Numbers, String, Arrays, NumericArrays...
+ Association / ReadOnlyVariableBinding (Globales, classvars, poolDict elements)
+ literales arbitrarios (`code`) - PERO NO TENEMOS COMPILADOR!!!!!!!!!!!!!!!!!! 
	Usar `ReferenceStream streamedRepresentationOf: aLiteral` y después `ReferenceStream unStream: aByteArray`?"! !

!Noclash_PointCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:39'!
x
^x! !

!Noclash_PointCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:39'!
y
^y! !

!Noclash_PointCodeModelExperiment methodsFor: 'private - initialization' stamp: 'jmv 8/2/2023 16:44:39'!
initializePvtX: xValue y: yValue
	"Points are immutable."
	x := xValue.
	y := yValue! !

!Noclash_PointCodeModelExperiment methodsFor: 'printing' stamp: 'jmv 8/2/2023 16:44:39'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y negative ifTrue: [ aStream space ].
	y printOn: aStream! !

!Noclash_PointCodeModelExperiment class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 16:44:39'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."

	^self new initializePvtX: anX y: anY! !

!CodeModelTest1 methodsFor: 'testing' stamp: 'jmv 9/16/2025 13:04:19'!
testAllInstanceSideMethods
	| classSpecs cmSpecs methods newMethods |
	classSpecs := OrderedDictionary new.
	Object withAllSubclassesDo: [ :cls |
		cls isMeta ifFalse: [
			"Methods in libraries will answer false just for that."
			methods := (cls methodDict select: [ :cm | (cm propertyValueAt: #libraryName) isNil ]) asArray.
			cmSpecs := OrderedCollection new.
			methods do: [ :cm |
				cmSpecs add: (MethodSpec for: cm addClassesTo: classSpecs) ].
			newMethods := cmSpecs collect: [ :methodSpec | methodSpec materializeIn: cls ].
			newMethods with: methods do: [ :newCm :cm |
				self assert: newCm = cm ]]]! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:24'!
testCodeModelExperiment
	| lib oldClass newClass |
	Smalltalk at: #CodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_CodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #() )
		instanceSelectors: #( #( method01) ).
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #CodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #method01) =
		(newClass compiledMethodAt: #method01).
	self assert: Noclash_CodeModelExperiment new method01 = newClass new method01.
	newClass removeFromSystemUnlogged.! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:33'!
testFloatArray
	| lib oldClass newClass |
	Smalltalk at: #Float32ArrayCodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_Float32ArrayCodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #() )
		instanceSelectors: #( #(floatAt: floatAt:put:) ).	
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #Float32ArrayCodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #floatAt:) =
		(newClass compiledMethodAt: #floatAt:).
	self assert: ((newClass with: 1.23) + (newClass with: 2.77)) first = 4.0.
	self assert: (newClass with: 1.23 with: 2.77) sum = 4.0.
	newClass removeFromSystemUnlogged.! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:40'!
testPoint
	| lib oldClass newClass |
	Smalltalk at: #PointCodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_PointCodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #(x:y:) )
		instanceSelectors: #( #( initializePvtX:y: x y #printOn:) ).
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #PointCodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #initializePvtX:y:) =
		(newClass compiledMethodAt: #initializePvtX:y:).
	self assert: (newClass x: 99 y: 86) printString = '99@86'.
	self assert: (newClass x: 99 y: 86) x = 99.
	self assert: (newClass x: 99 y: 86) y = 86.
	newClass removeFromSystemUnlogged.! !

!DclTest methodsFor: 'tests 01' stamp: 'jmv 10/24/2023 10:17:34'!
test01NewClass
	"The library includes only a single class.
	Test creation of a simple class with a few instance variables and methods that access them"

	| sourceForLibrary lib installedFromLibrary |
	Smalltalk at: #DclExample01 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForLibrary := Noclash_DclExample01.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample01.
	self assert: installedFromLibrary superclass == Object.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary classVarNames equals: sourceForLibrary classVarNames.
	self assert: installedFromLibrary class instVarNames equals: sourceForLibrary class instVarNames.
	self assert: installedFromLibrary selectors equals: sourceForLibrary selectors.
	self assert: installedFromLibrary class selectors equals: sourceForLibrary class selectors.
	self assert: installedFromLibrary class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedFromLibrary class >> #processor equals: sourceForLibrary class >> #processor.
	self assert: installedFromLibrary >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForLibrary >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForLibrary >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForLibrary >> #c.
	self assert: installedFromLibrary >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedFromLibrary new combined equals: 123.
	self assert: installedFromLibrary new maxVal equals: SmallInteger maxVal.
	self assert: installedFromLibrary new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedFromLibrary processor equals: Processor.
	installedFromLibrary setClassIVars.
	self assert: installedFromLibrary combinedCIvars equals: 57.

	installedFromLibrary removeFromSystemUnlogged.! !

!DclTest methodsFor: 'tests 02' stamp: 'jmv 10/24/2023 12:28:42'!
test02ExistingClassSameShape
	"The library includes only a single class.
	The class already exist before loading the library, and with the same shape.
	Any existing methods are exactly the same.
	Test that class shape and methods are healthy."

	| sourceForExisting existing sourceForLibrary lib installedFromLibrary |
	"Create pre-existing version of the class"
	Smalltalk at: #DclExample02 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashExisting_DclExample02.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.

	existing := Smalltalk at: #DclExample02.
	self assert: existing new combined equals: 47.
	self assert: existing new maxVal equals: SmallInteger maxVal-1.
	self assert: existing new someLiteral equals: #(22 33).
	self assert: existing processor equals: nil.
	existing setClassIVars.
	self assert: existing combinedCIvars equals: 7.

	"Create library"
	sourceForLibrary := Noclash_DclExample02.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample02.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary selectors asSet equals: (sourceForExisting selectors, sourceForLibrary selectors) asSet.
	self assert: installedFromLibrary class selectors equals: sourceForLibrary class selectors.
	self assert: installedFromLibrary class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedFromLibrary class >> #processor equals: sourceForLibrary class >> #processor.
	self assert: installedFromLibrary >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForLibrary >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForLibrary >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForLibrary >> #c.
	self assert: (installedFromLibrary >> #combined) ~= (sourceForExisting >> #combined).
	self assert: installedFromLibrary >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedFromLibrary new combined equals: 123.
	self assert: installedFromLibrary new maxVal equals: SmallInteger maxVal.
	self assert: installedFromLibrary new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedFromLibrary processor equals: Processor.
	self assert: installedFromLibrary class instVarNames equals: sourceForLibrary class instVarNames.
	installedFromLibrary setClassIVars.
	self assert: installedFromLibrary combinedCIvars equals: 57.

	"Cleanup"
	Smalltalk at: #DclExample02 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 10/25/2023 10:48:09'!
test03AExistingClassNewClassIVars
	"The existing class and its definition in the library differ in classInstanceVariables
	Installation should fail."

	| sourceForExisting existing sourceForLibrary lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABCnoClassIvars_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample03.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	self assert: existing class allClassInstVarNames equals: sourceForExisting class allClassInstVarNames.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self deny: lib canBeInstalled.

	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:24:59'!
test03AExistingClassNewIVarsExistingInstances
	"The library adds an instance variable to a class that has existing instances.
	Installation should fail."

	| sourceForExisting existing sourceForLibrary lib anInstance |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashAB_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample03.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	anInstance := existing new.
	self assert: anInstance combined equals: 79.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self deny: lib canBeInstalled.
	"Ensure that the instance is still around"
	self assert: anInstance notNil.

	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 12:38:09'!
test03BExistingClassNewIVarsShouldMerge
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample03.
	sourceForExistingSubclass := NoclashABD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample03.
	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 12:40:53'!
test03CExistingClassNewIVarsShouldConvertExistingMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables.
	Existing methods in the superclass should remain unchanged, as new instance variables are added at the end."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample03.
	sourceForExistingSubclass := NoclashABD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSuperclass new xk47 equals: 'xk47'.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.
	self assert: existingSubclass new xk47 equals: 'xk47'.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample03.
	installedSubclass := Smalltalk at: #DclExample03Subclass.

	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	self assert: installedSuperclass selectors asSet equals:
		(sourceForExistingSuperclass selectors, sourceForLibrary selectors) asSet.
	self assert: installedSuperclass class selectors asSet equals:
		(sourceForExistingSuperclass class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedSuperclass class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedSuperclass >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedSuperclass >> #a equals: sourceForLibrary >> #a.
	self assert: installedSuperclass >> #b equals: sourceForLibrary >> #b.
	self assert: installedSuperclass >> #c equals: sourceForLibrary >> #c.
	self deny: (installedSuperclass >> #combined) = (sourceForExistingSuperclass >> #combined).
	self assert: installedSuperclass >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedSuperclass >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedSuperclass >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedSuperclass >> #xk47 equals: sourceForExistingSuperclass >> #xk47.

	self assert: installedSubclass selectors equals: sourceForExistingSubclass selectors.
	self assert: installedSubclass class selectors equals: sourceForExistingSubclass class selectors.
	self deny: (installedSubclass >> #initialize) = (sourceForExistingSubclass >> #initialize)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #d) = (sourceForExistingSubclass >> #d)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #combined) = (sourceForExistingSubclass >> #combined)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.

	self assert: installedSuperclass new a equals: 1.
	self assert: installedSuperclass new b equals: 2.
	self assert: installedSuperclass new c equals: 3.
	self assert: installedSuperclass new combined equals: 123.
	self assert: installedSuperclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSuperclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSuperclass new xk47 equals: 'xk47'.

	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.
	self assert: installedSubclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSubclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSubclass new xk47 equals: 'xk47'.

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:25:13'!
test03DExistingClassOldIvarMissingShouldKeepShape
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Shape of the class should not change."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample03.
	sourceForExistingSubclass := NoclashABCD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new b equals: 2.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable."
	sourceForLibrarySubclass := NoclashABD_DclExample03Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.
	
	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:39:36'!
test03EExistingClassOldIvarMissingShouldConvertNewMethods
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Methods in the subclass that came from the library should be adjusted to access the correct instance variables,
		because they were created unaware of the 'missing' instance variable."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample03.
	sourceForExistingSubclass := NoclashABCD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new b equals: 2.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable (in the superclass actually).
	Neither class shape should be modified."
	sourceForLibrarySubclass := NoclashABD_DclExample03Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:25:23'!
test03FExistingClassReorderedIVarsShouldConvertNewMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables."

	| sourceForExisting existingClass sourceForLibrary lib installedFromLibrary |

	"Create pre-existing version of the class."
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashACB_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existingClass := Smalltalk at: #DclExample03.
	self assert: existingClass allInstVarNames equals: #('a' 'c' 'b').

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample03.

	self assert: installedFromLibrary allInstVarNames equals: #('a' 'c' 'b').
	self assert: installedFromLibrary new initialize2 a equals: 1.
	self assert: installedFromLibrary new initialize2 b equals: 2.
	self assert: installedFromLibrary new initialize2 c equals: 3.
	self assert: installedFromLibrary new initialize2 combined equals: 123.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 04' stamp: 'jmv 8/11/2023 10:25:35'!
test04AExistingClassNewIVarsAlreadyInSuper
	"The library includes an instance variable that is already defined in a superclass in the image.
	Installation should fail."

	| sourceForExisting existing sourceForLibrarySuperclass sourceForLibrarySubclass lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABC_DclExample04.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.
	existing := Smalltalk at: #DclExample04.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.

	"Create library"
	sourceForLibrarySuperclass := NoclashAC_DclExample04.
	sourceForLibrarySubclass := NoclashB_DclExample04Sub.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySuperclass.  sourceForLibrarySubclass }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 04' stamp: 'jmv 8/11/2023 10:26:24'!
test04BExistingClassNewIVarsAlreadyInSub
	"The library includes an instance variable that is already defined in a subclass in the image.
	Installation should fail."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass sourceForLibrary lib |

	"Create pre-existing version of the classes"
	Smalltalk at: #DclExample04Sub ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAC_DclExample04.
	sourceForExistingSubclass := NoclashB_DclExample04Sub.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass.  sourceForExistingSubclass }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample04.
	existingSubclass := Smalltalk at: #DclExample04Sub.
	self assert: existingSuperclass instVarNames equals: sourceForExistingSuperclass instVarNames.
	self assert: existingSubclass instVarNames equals: sourceForExistingSubclass instVarNames.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample04.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample04Sub ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:20'!
test05ADifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:25:59'!
test05BDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashProtoObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:04'!
test05CDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:08'!
test05DDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashProtoObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:13'!
test05EDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.
	(Smalltalk at: #DclExample05) superclass: nil.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:03'!
test05FDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:15'!
test05GDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:21'!
test05HDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:26'!
test05IDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:31'!
test05JDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:37'!
test05KDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:55'!
test05LDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 06' stamp: 'jmv 8/14/2023 15:48:32'!
test06MissingGlobal
	"Verify that unknown variables are created as Globals (not as Undeclared), with a nil value."

	| rsg lib installedFromLibrary |
	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk removeKey: #DclExample06Global ifAbsent: nil.
	Smalltalk cleanOutUndeclared.

	Smalltalk at: #DclExample06Global put: 123.4.
	rsg := Smalltalk at: #LogCompilerUse.
	Smalltalk at: #LogCompilerUse put: false.
	Object subclass: #DclExample06
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'TestDynamicCuisLibraries-Transient'.
	(Smalltalk at: #DclExample06) compile: 'a1 ^DclExample06Global'.
	Smalltalk at: #LogCompilerUse put: rsg.
	self assert: (Smalltalk at: #DclExample06) new a1 = 123.4.	

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { #DclExample06 }
		classSelectors: { {} }
		instanceSelectors: { {#a1} }.

	(Smalltalk at: #DclExample06) removeFromSystemUnlogged.
	Smalltalk removeKey: #DclExample06Global.
	Smalltalk cleanOutUndeclared.

	self deny: (Smalltalk includesKey: #DclExample06Global) description: 'Precondition for this test failed. Please check test itself.'.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample06.
	self assert: (Smalltalk includesKey: #DclExample06Global) description: 'Global should have been created automatically on method materialization'.
	self assert: installedFromLibrary new a1 equals: nil.	
	Smalltalk at: #DclExample06Global put: 1234.56.
	self assert: installedFromLibrary new a1 equals: 1234.56.	

	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk removeKey: #DclExample06Global ifAbsent: nil.
	Smalltalk cleanOutUndeclared.! !

!DclTest methodsFor: 'tests 06' stamp: 'jmv 8/11/2023 12:34:16'!
test06NewClassVar
	| sourceForExisting existing sourceForLibrary lib anInstance installedFromLibrary |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABC_DclExample06.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample06.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	self assert: existing selectors equals: sourceForExisting selectors.
	self assert: existing class selectors equals: existing class selectors.
	self assert: existing >> #initialize equals: sourceForExisting >> #initialize.
	self assert: existing >> #a equals: sourceForExisting >> #a.
	self assert: existing >> #b equals: sourceForExisting >> #b.
	self assert: existing >> #c equals: sourceForExisting >> #c.
	self assert: existing >> #combined equals: sourceForExisting >> #combined.
	self assert: existing >> #maxVal equals: sourceForExisting >> #maxVal.
	self assert: existing >> #someLiteral equals: sourceForExisting >> #someLiteral.

	anInstance := existing new.
	self assert: anInstance a equals: 1.
	self assert: anInstance b equals: 2.
	self assert: anInstance c equals: 3.
	self assert: anInstance combined equals: 123.
	self assert: anInstance maxVal equals: SmallInteger maxVal.
	self assert: anInstance someLiteral equals: NoclashABC_DclExample06 new someLiteral.

	"Create library"
	sourceForLibrary := NoclashABCclasVarZZ_DclExample06.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	"Ensure that the instance is still around"
	self assert: anInstance notNil.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample06.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary selectors asSet equals:
		(sourceForExisting selectors, sourceForLibrary selectors) asSet.
	self assert: installedFromLibrary class selectors asSet equals:
		(sourceForExisting class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedFromLibrary >> #initialize equals: sourceForExisting >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForExisting >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForExisting >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForExisting >> #c.
	self assert: installedFromLibrary >> #combined equals: sourceForExisting >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForExisting >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForExisting >> #someLiteral.
	self assert: installedFromLibrary >> #setAndAnswerZZ equals: sourceForLibrary >> #setAndAnswerZZ.

	self assert: installedFromLibrary setAndAnswerZZ2 = 12742.
	self assert: anInstance setAndAnswerZZ = 1274.
	self assert: anInstance a equals: 1.
	self assert: anInstance b equals: 2.
	self assert: anInstance c equals: 3.
	self assert: anInstance combined equals: 123.
	self assert: anInstance maxVal equals: SmallInteger maxVal.
	self assert: anInstance someLiteral equals: NoclashABC_DclExample06 new someLiteral.

	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 07' stamp: 'jmv 7/30/2025 12:38:34'!
test07CExistingClassNewIVarsShouldConvertExistingMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables.
	Existing methods in the superclass should remain unchanged, as new instance variables are added at the end.
	Similar to test03CExistingClassNewIVarsShouldConvertExistingMethods, but exercising the full 1 byte index range."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample07Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample07 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample07.
	sourceForExistingSubclass := NoclashABD_DclExample07Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample07.
	existingSubclass := Smalltalk at: #DclExample07Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240').
	self assert: existingSubclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSuperclass new xk47 equals: 'xk47'.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.
	self assert: existingSubclass new xk47 equals: 'xk47'.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample07.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample07.
	installedSubclass := Smalltalk at: #DclExample07Subclass.

	self assert: installedSuperclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c').
	self assert: installedSubclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c' 'd').

	self assert: installedSuperclass selectors asSet equals:
		(sourceForExistingSuperclass selectors, sourceForLibrary selectors) asSet.
	self assert: installedSuperclass class selectors asSet equals:
		(sourceForExistingSuperclass class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedSuperclass class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedSuperclass >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedSuperclass >> #a equals: sourceForLibrary >> #a.
	self assert: installedSuperclass >> #b equals: sourceForLibrary >> #b.
	self assert: installedSuperclass >> #c equals: sourceForLibrary >> #c.
	self deny: (installedSuperclass >> #combined) = (sourceForExistingSuperclass >> #combined).
	self assert: installedSuperclass >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedSuperclass >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedSuperclass >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedSuperclass >> #xk47 equals: sourceForExistingSuperclass >> #xk47.

	self assert: installedSubclass selectors equals: sourceForExistingSubclass selectors.
	self assert: installedSubclass class selectors equals: sourceForExistingSubclass class selectors.
	self deny: (installedSubclass >> #initialize) = (sourceForExistingSubclass >> #initialize)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #d) = (sourceForExistingSubclass >> #d)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #combined) = (sourceForExistingSubclass >> #combined)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.

	self assert: installedSuperclass new a equals: 1.
	self assert: installedSuperclass new b equals: 2.
	self assert: installedSuperclass new c equals: 3.
	self assert: installedSuperclass new combined equals: 123.
	self assert: installedSuperclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSuperclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSuperclass new xk47 equals: 'xk47'.

	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.
	self assert: installedSubclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSubclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSubclass new xk47 equals: 'xk47'.

	Smalltalk at: #DclExample07Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample07 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 07' stamp: 'jmv 7/30/2025 12:43:28'!
test07EExistingClassOldIvarMissingShouldConvertNewMethods
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Methods in the subclass that came from the library should be adjusted to access the correct instance variables,
		because they were created unaware of the 'missing' instance variable.
	Similar to test03EExistingClassOldIvarMissingShouldConvertNewMethods, but exercising the full 1 byte index range."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample07Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample07 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample07.
	sourceForExistingSubclass := NoclashABCD_DclExample07Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample07.
	existingSubclass := Smalltalk at: #DclExample07Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c').
	self assert: existingSubclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new b equals: 2.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable (in the superclass actually).
	Neither class shape should be modified."
	sourceForLibrarySubclass := NoclashABD_DclExample07Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample07Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c').
	self assert: installedSubclass allInstVarNames equals: #('z1' 'z2' 'z3' 'z4' 'z5' 'z6' 'z7' 'z8' 'z9' 'a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.

	Smalltalk at: #DclExample07Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample07 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 08' stamp: 'jmv 7/31/2025 13:05:04'!
test08CExistingClassNewIVarsShouldConvertExistingMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables.
	Existing methods in the superclass should remain unchanged, as new instance variables are added at the end.
	Similar to test07CExistingClassNewIVarsShouldConvertExistingMethods, but converting between 1-byte and 2-byte opcodes,
	something that can only be done with the Sista Bytecodes Set."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample08Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample08 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample08.
	sourceForExistingSubclass := NoclashABD_DclExample08Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample08.
	existingSubclass := Smalltalk at: #DclExample08Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSuperclass new xk47 equals: 'xk47'.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.
	self assert: existingSubclass new xk47 equals: 'xk47'.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample08.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample08.
	installedSubclass := Smalltalk at: #DclExample08Subclass.

	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'c' 'd').

	self assert: installedSuperclass selectors asSet equals:
		(sourceForExistingSuperclass selectors, sourceForLibrary selectors) asSet.
	self assert: installedSuperclass class selectors asSet equals:
		(sourceForExistingSuperclass class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedSuperclass class >> #new equals: sourceForLibrary class >> #new.
	self deny: (installedSuperclass >> #initialize) = (sourceForLibrary >> #initialize)
		description: 'Installed methods should be corrected for class shape due to different shape if base system'.
	self assert: installedSuperclass >> #a equals: sourceForLibrary >> #a.
	self assert: installedSuperclass >> #c equals: sourceForLibrary >> #c.
	self deny: (installedSuperclass >> #combined) = (sourceForExistingSuperclass >> #combined).
	self deny: (installedSuperclass >> #combined) = (sourceForLibrary >> #combined)
		description: 'Installed methods should be corrected for class shape due to different shape if base system'.
	self assert: installedSuperclass >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedSuperclass >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedSuperclass >> #xk47 equals: sourceForExistingSuperclass >> #xk47.

	self assert: installedSubclass selectors equals: sourceForExistingSubclass selectors.
	self assert: installedSubclass class selectors equals: sourceForExistingSubclass class selectors.
	self deny: (installedSubclass >> #initialize) = (sourceForExistingSubclass >> #initialize)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #d) = (sourceForExistingSubclass >> #d)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #combined) = (sourceForExistingSubclass >> #combined)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.

	self assert: installedSuperclass new a equals: 1.
	self assert: installedSuperclass new b equals: 2.
	self assert: installedSuperclass new c equals: 3.
	self assert: installedSuperclass new combined equals: 123.
	self assert: installedSuperclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSuperclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSuperclass new xk47 equals: 'xk47'.

	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.
	self assert: installedSubclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSubclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSubclass new xk47 equals: 'xk47'.

	Smalltalk at: #DclExample08Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample08 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 08' stamp: 'jmv 7/31/2025 12:59:47'!
test08EExistingClassOldIvarMissingShouldConvertNewMethods
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Methods in the subclass that came from the library should be adjusted to access the correct instance variables,
		because they were created unaware of the 'missing' instance variable.
	Similar to test07EExistingClassOldIvarMissingShouldConvertNewMethods, but converting between 1-byte and 2-byte opcodes,
	something that can only be done with the Sista Bytecodes Set."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample08Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample08 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample08.
	sourceForExistingSubclass := NoclashABCD_DclExample08Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample08.
	existingSubclass := Smalltalk at: #DclExample08Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable (in the superclass actually).
	Neither class shape should be modified."
	sourceForLibrarySubclass := NoclashABD_DclExample08Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample08Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.

	Smalltalk at: #DclExample08Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample08 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 09' stamp: 'jmv 9/23/2025 17:51:25'!
test09CExistingClassNewIVarsShouldConvertExistingMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables.
	Existing methods in the superclass should remain unchanged, as new instance variables are added at the end.
	Similar to test08CExistingClassNewIVarsShouldConvertExistingMethods, but converting between 1-byte and 4-byte opcodes,
	something that can only be done with the Sista Bytecodes Set."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample09Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample09 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample09.
	sourceForExistingSubclass := NoclashABD_DclExample09Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample09.
	existingSubclass := Smalltalk at: #DclExample09Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSuperclass new xk47 equals: 'xk47'.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.
	self assert: existingSubclass new xk47 equals: 'xk47'.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample09.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample09.
	installedSubclass := Smalltalk at: #DclExample09Subclass.

	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'c' 'd').

	self assert: installedSuperclass selectors asSet equals:
		(sourceForExistingSuperclass selectors, sourceForLibrary selectors) asSet.
	self assert: installedSuperclass class selectors asSet equals:
		(sourceForExistingSuperclass class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedSuperclass class >> #new equals: sourceForLibrary class >> #new.
	self deny: (installedSuperclass >> #initialize) = (sourceForLibrary >> #initialize)
		description: 'Installed methods should be corrected for class shape due to different shape if base system'.
	self assert: installedSuperclass >> #a equals: sourceForLibrary >> #a.
	self assert: installedSuperclass >> #c equals: sourceForLibrary >> #c.
	self deny: (installedSuperclass >> #combined) = (sourceForExistingSuperclass >> #combined).
	self deny: (installedSuperclass >> #combined) = (sourceForLibrary >> #combined)
		description: 'Installed methods should be corrected for class shape due to different shape if base system'.
	self assert: installedSuperclass >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedSuperclass >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedSuperclass >> #xk47 equals: sourceForExistingSuperclass >> #xk47.

	self assert: installedSubclass selectors equals: sourceForExistingSubclass selectors.
	self assert: installedSubclass class selectors equals: sourceForExistingSubclass class selectors.
	self deny: (installedSubclass >> #initialize) = (sourceForExistingSubclass >> #initialize)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #d) = (sourceForExistingSubclass >> #d)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #combined) = (sourceForExistingSubclass >> #combined)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.

	self assert: installedSuperclass new a equals: 1.
	self assert: installedSuperclass new b equals: 2.
	self assert: installedSuperclass new c equals: 3.
	self assert: (installedSuperclass new instVarNamed: 'a') equals: 1.
	self assert: (installedSuperclass new instVarNamed: 'b') equals: 2.
	self assert: (installedSuperclass new instVarNamed: 'c') equals: 3.
	self assert: installedSuperclass new combined equals: 123.
	self assert: installedSuperclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSuperclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSuperclass new xk47 equals: 'xk47'.

	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: (installedSubclass new instVarNamed: 'a') equals: 1.
	self assert: (installedSubclass new instVarNamed: 'b') equals: 2.
	self assert: (installedSubclass new instVarNamed: 'c') equals: 3.
	self assert: (installedSubclass new instVarNamed: 'd') equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.
	self assert: installedSubclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSubclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSubclass new xk47 equals: 'xk47'.

	Smalltalk at: #DclExample09Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample09 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 09' stamp: 'jmv 9/23/2025 17:51:57'!
test09EExistingClassOldIvarMissingShouldConvertNewMethods
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Methods in the subclass that came from the library should be adjusted to access the correct instance variables,
		because they were created unaware of the 'missing' instance variable.
	Similar to test08EExistingClassOldIvarMissingShouldConvertNewMethods, but converting between 1-byte and 4-byte opcodes,
	something that can only be done with the Sista Bytecodes Set."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample09Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample09 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample09.
	sourceForExistingSubclass := NoclashABCD_DclExample09Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample09.
	existingSubclass := Smalltalk at: #DclExample09Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable (in the superclass actually).
	Neither class shape should be modified."
	sourceForLibrarySubclass := NoclashABD_DclExample09Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample09Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'a1' 'a2' 'a3' 'a4' 'a5' 'a6' 'a7' 'a8' 'a9' 'a10' 'a11' 'a12' 'a13' 'a14' 'a15' 'a16' 'a17' 'a18' 'a19' 'a20' 'a21' 'a22' 'a23' 'a24' 'a25' 'a26' 'a27' 'a28' 'a29' 'a30' 'a31' 'a32' 'a33' 'a34' 'a35' 'a36' 'a37' 'a38' 'a39' 'a40' 'a41' 'a42' 'a43' 'a44' 'a45' 'a46' 'a47' 'a48' 'a49' 'a50' 'a51' 'a52' 'a53' 'a54' 'a55' 'a56' 'a57' 'a58' 'a59' 'a60' 'a61' 'a62' 'a63' 'a64' 'a65' 'a66' 'a67' 'a68' 'a69' 'a70' 'a71' 'a72' 'a73' 'a74' 'a75' 'a76' 'a77' 'a78' 'a79' 'a80' 'a81' 'a82' 'a83' 'a84' 'a85' 'a86' 'a87' 'a88' 'a89' 'a90' 'a91' 'a92' 'a93' 'a94' 'a95' 'a96' 'a97' 'a98' 'a99' 'a100' 'a101' 'a102' 'a103' 'a104' 'a105' 'a106' 'a107' 'a108' 'a109' 'a110' 'a111' 'a112' 'a113' 'a114' 'a115' 'a116' 'a117' 'a118' 'a119' 'a120' 'a121' 'a122' 'a123' 'a124' 'a125' 'a126' 'a127' 'a128' 'a129' 'a130' 'a131' 'a132' 'a133' 'a134' 'a135' 'a136' 'a137' 'a138' 'a139' 'a140' 'a141' 'a142' 'a143' 'a144' 'a145' 'a146' 'a147' 'a148' 'a149' 'a150' 'a151' 'a152' 'a153' 'a154' 'a155' 'a156' 'a157' 'a158' 'a159' 'a160' 'a161' 'a162' 'a163' 'a164' 'a165' 'a166' 'a167' 'a168' 'a169' 'a170' 'a171' 'a172' 'a173' 'a174' 'a175' 'a176' 'a177' 'a178' 'a179' 'a180' 'a181' 'a182' 'a183' 'a184' 'a185' 'a186' 'a187' 'a188' 'a189' 'a190' 'a191' 'a192' 'a193' 'a194' 'a195' 'a196' 'a197' 'a198' 'a199' 'a200' 'a201' 'a202' 'a203' 'a204' 'a205' 'a206' 'a207' 'a208' 'a209' 'a210' 'a211' 'a212' 'a213' 'a214' 'a215' 'a216' 'a217' 'a218' 'a219' 'a220' 'a221' 'a222' 'a223' 'a224' 'a225' 'a226' 'a227' 'a228' 'a229' 'a230' 'a231' 'a232' 'a233' 'a234' 'a235' 'a236' 'a237' 'a238' 'a239' 'a240' 'a241' 'a242' 'a243' 'a244' 'a245' 'a246' 'a247' 'a248' 'a249' 'a250' 'a251' 'a252' 'a253' 'a254' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new c equals: 3.
	self assert: (existingSuperclass new instVarNamed: 'a') equals: 1.
	self assert: (existingSuperclass new instVarNamed: 'b') equals: 2.
	self assert: (existingSuperclass new instVarNamed: 'c') equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: (installedSubclass new instVarNamed: 'a') equals: 1.
	self assert: (installedSubclass new instVarNamed: 'b') equals: 2.
	self assert: (installedSubclass new instVarNamed: 'c') equals: 3.
	self assert: (installedSubclass new instVarNamed: 'd') equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.

	Smalltalk at: #DclExample09Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample09 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/9/2023 16:09:45'!
setUp
	Smalltalk at: #LogCompilerUse put: true.
	Smalltalk at: #CompilerWasUsed put: false.! !

!DclTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/8/2023 10:46:31'!
tearDown
	LogCompilerUse := false! !

!DclTest methodsFor: 'failing' stamp: 'jmv 7/31/2025 14:58:59'!
expectedFailures
	"Answer an array o selectors that are expected to fail.
	Usually this is done for tests that document incorrect behavior for which there is not yet a proper fix available."

	^CompiledMethod preferredBytecodeSetEncoderClass name == #EncoderForSistaV1
		ifTrue: [#()]
		ifFalse: [#(test08EExistingClassOldIvarMissingShouldConvertNewMethods)]! !

!DclTest class methodsFor: 'class initialization' stamp: 'jmv 10/19/2023 15:37:34'!
initialize

	Smalltalk at: #LogCompilerUse put: false.
	Smalltalk at: #CompilerWasUsed put: false.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:38:50'!
testInstVarRemapping01
	"
	Excercise remapping. Stay in the low (1 byte) area.
	InstvarRemappingTest new setUp testInstVarRemapping01
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test03AccessHighVariables.
	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls new test03AccessHighVariables.

	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {255->234} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a234 := 99 ]
		ifFalse: [ a234 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a234 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test03AccessHighVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:39:04'!
testInstVarRemapping02
	"
	Excercise remapping. Stay in the 2 byte area.
	InstvarRemappingTest new setUp testInstVarRemapping02
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test03AccessHighVariables.
	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls new test03AccessHighVariables.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {258->262} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a262 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a262 = #[11 22 33 44].'.
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test03AccessHighVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:40:02'!
testInstVarRemapping03
	"
	Excercise remapping. Cross the 256 ivar barrier.
	InstvarRemappingTest new setUp testInstVarRemapping03
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test03AccessHighVariables.
	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls new test03AccessHighVariables.

	"Needs to convert 2 byte bytecodes into 4 byte bytecodes"
	self shouldFail: [ EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {255->262} asDictionary doIt: true ].
	
	method := method encoderClass withLongInstVarAccessBytecodes: method.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {255->262} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a262 := 99 ]
		ifFalse: [ a262 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a262 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {262->255} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test03AccessHighVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:40:07'!
testInstVarRemapping04
	"
	Excercise remapping. Cross the 256 ivar barrier.
	InstvarRemappingTest new setUp testInstVarRemapping04
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test03AccessHighVariables.
	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a255 := 99 ]
		ifFalse: [ a255 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a255 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls new test03AccessHighVariables.

	"Needs to convert 2 byte bytecodes into 4 byte bytecodes"
	self shouldFail: [ EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {255->262} asDictionary doIt: true ].
	
	method := method encoderClass withLongInstVarAccessBytecodes: method.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {255->262} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a262 := 99 ]
		ifFalse: [ a262 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a262 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {262->6} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test03AccessHighVariables
	1 > 0
		ifTrue: [ a6 := 99 ]
		ifFalse: [ a6 := 98 ].
	a256 := ''a1''.
	a257 := Float pi.
	a258 := #[11 22 33 44].
	self assert: a6 = 99.
	self assert: a256 = ''a1''.
	self assert: a257 = Float pi.
	self assert: a258 = #[11 22 33 44].'.
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test03AccessHighVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:40:18'!
testInstVarRemapping05
	"
	Excercise remapping. Go from short form to full 1 byte.
	InstvarRemappingTest new setUp testInstVarRemapping05
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test01AccessLowVariables.
	self assert: method decompile decompileString = 'test01AccessLowVariables
	1 > 0
		ifTrue: [ a5 := 99 ]
		ifFalse: [ a5 := 98 ].
	a6 := ''a1''.
	a7 := Float pi.
	a8 := #[11 22 33 44].
	self assert: a5 = 99.
	self assert: a6 = ''a1''.
	self assert: a7 = Float pi.
	self assert: a8 = #[11 22 33 44].' .
	
	cls new test01AccessLowVariables.

	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {8->234} asDictionary doIt: true.

	self assert: method decompile decompileString = 'test01AccessLowVariables
	1 > 0
		ifTrue: [ a5 := 99 ]
		ifFalse: [ a5 := 98 ].
	a6 := ''a1''.
	a7 := Float pi.
	a234 := #[11 22 33 44].
	self assert: a5 = 99.
	self assert: a6 = ''a1''.
	self assert: a7 = Float pi.
	self assert: a234 = #[11 22 33 44].' .
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test01AccessLowVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:40:23'!
testInstVarRemapping06
	"
	Excercise remapping. Go from short form to over 256.
	InstvarRemappingTest new setUp testInstVarRemapping06
	"
	| cls method |
	cls := Smalltalk at: #LargeClassIvarR01.
	method := cls >> #test01AccessLowVariables.
	self assert: method decompile decompileString = 'test01AccessLowVariables
	1 > 0
		ifTrue: [ a5 := 99 ]
		ifFalse: [ a5 := 98 ].
	a6 := ''a1''.
	a7 := Float pi.
	a8 := #[11 22 33 44].
	self assert: a5 = 99.
	self assert: a6 = ''a1''.
	self assert: a7 = Float pi.
	self assert: a8 = #[11 22 33 44].' .
	
	cls new test01AccessLowVariables.

	self shouldFail: [ EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {8->261} asDictionary doIt: true ].

	method := method encoderClass withLongInstVarAccessBytecodes: method.
	
	EncoderForSistaV1 remapInstVarAccessesIn: method asPer: {8->261} asDictionary doIt: true.
	
	self assert: method decompile decompileString = 'test01AccessLowVariables
	1 > 0
		ifTrue: [ a5 := 99 ]
		ifFalse: [ a5 := 98 ].
	a6 := ''a1''.
	a7 := Float pi.
	a261 := #[11 22 33 44].
	self assert: a5 = 99.
	self assert: a6 = ''a1''.
	self assert: a7 = Float pi.
	self assert: a261 = #[11 22 33 44].' .
	
	cls addSelectorSilently: method selector withMethod: method.
	cls new test01AccessLowVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:41:26'!
testLongInstVarAcessConversion01
	"
	InstvarRemappingTest new setUp testLongInstVarAcessConversion01
	"
	| cls original converted |
	cls := Smalltalk at: #LargeClassIvarR01.
	original := cls >> #test01AccessLowVariables.
	converted := original encoderClass withLongInstVarAccessBytecodes: original.
	
	self assert: original symbolic = self m01Bytecode.
	self assert: converted symbolic = self m01BytecodeLongIvarAcessAndJumps.

	cls new test01AccessLowVariables.
	cls addSelectorSilently: converted selector withMethod: converted.
	cls new test01AccessLowVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:42:24'!
testLongInstVarAcessConversion02
	"
	InstvarRemappingTest new setUp testLongInstVarAcessConversion02
	"
	| cls original converted |
	cls := Smalltalk at: #LargeClassIvarR01.
	original := cls >> #test02AccessMidVariables.
	converted := original encoderClass withLongInstVarAccessBytecodes: original.
	
	self assert: original symbolic = self m02Bytecode.
	self assert: converted symbolic = self m02BytecodeLongIvarAcessAndJumps.

	cls new test02AccessMidVariables.
	cls addSelectorSilently: converted selector withMethod: converted.
	cls new test02AccessMidVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:42:35'!
testLongInstVarAcessConversion03
	"
	InstvarRemappingTest new setUp testLongInstVarAcessConversion03
	"
	| cls original converted |
	cls := Smalltalk at: #LargeClassIvarR01.
	original := cls >> #test03AccessHighVariables.
	converted := original encoderClass withLongInstVarAccessBytecodes: original.
	
	self assert: original symbolic = self m03Bytecode.
	self assert: converted symbolic = self m03BytecodeLongIvarAcessAndJumps.

	cls new test03AccessHighVariables.
	cls addSelectorSilently: converted selector withMethod: converted.
	cls new test03AccessHighVariables.! !

!InstvarRemappingTest methodsFor: 'tests' stamp: 'jmv 8/1/2025 10:42:55'!
testLongInstVarAcessConversion04
	"
	InstvarRemappingTest new setUp testLongInstVarAcessConversion04
	"
	| cls original converted |
	cls := Smalltalk at: #LargeClassIvarR01.
	original := cls >> #test04LongJumps.
	converted := original encoderClass withLongInstVarAccessBytecodes: original.
	
	self assert: original symbolic = self m04Bytecode.
	self assert: converted symbolic = self m04BytecodeLongIvarAcessAndJumps.

	cls new test04LongJumps.
	cls addSelectorSilently: converted selector withMethod: converted.
	cls new test04LongJumps.! !

!InstvarRemappingTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/1/2025 10:56:38'!
setUp
	"Only for Sista!!
	Also see #expectedFailures"
	| cls |
	Smalltalk at: #LargeClassIvarR01 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	CompiledMethod preferredBytecodeSetEncoderClass name == #EncoderForSistaV1
		ifTrue: [
			Compiler evaluate: self classDeclarationString.
			cls := Smalltalk at: #LargeClassIvarR01.
			cls
				compile: self m01SourceCode notifying: nil;
				compile: self m02SourceCode notifying: nil;
				compile: self m03SourceCode notifying: nil;
				compile: self m04SourceCode notifying: nil ].! !

!InstvarRemappingTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/1/2025 09:41:48'!
tearDown
	"
	InstvarRemappingTest new setUp test01
	"
	Smalltalk at: #LargeClassIvarR01 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!InstvarRemappingTest methodsFor: 'subject code' stamp: 'jmv 8/1/2025 09:56:49'!
classDeclarationString
^ 'Object subclass: #LargeClassIvarR01
  	instanceVariableNames: ''a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83 a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a121 a122 a123 a124 a125 a126 a127 a128 a129 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a140 a141 a142 a143 a144 a145 a146 a147 a148 a149 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a160 a161 a162 a163 a164 a165 a166 a167 a168 a169 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a180 a181 a182 a183 a184 a185 a186 a187 a188 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a200 a201 a202 a203 a204 a205 a206 a207 a208 a209 a210 a211 a212 a213 a214 a215 a216 a217 a218 a219 a220 a221 a222 a223 a224 a225 a226 a227 a228 a229 a230 a231 a232 a233 a234 a235 a236 a237 a238 a239 a240 a241 a242 a243 a244 a245 a246 a247 a248 a249 a250 a251 a252 a253 a254 a255 a256 a257 a258 a259 a260 a261 a262''
  	classVariableNames: ''''
  	poolDictionaries: ''''
  	category: ''TestDynamicCuisLibraries-Transient'''! !

!InstvarRemappingTest methodsFor: 'subject code' stamp: 'jmv 8/1/2025 09:39:56'!
m01SourceCode
	^ 'test01AccessLowVariables
 
 	"Short #genStorePopInstVar: (up to 8 inst vars is Spur)"
 	1 > 0
 		ifTrue: [ a5 := 99 ]
 		ifFalse: [ a5 := 98 ].
 	a6 := ''a1''.
 	a7 := Float pi.
 	a8 := #[11 22 33 44].
 	
 	"Short #genPushInstVar: (up to 16 inst vars in Spur)"
 	self assert: a5 = 99.
 	self assert: a6 = ''a1''.
 	self assert: a7 = Float pi.
 	self assert: a8 = #[11 22 33 44].'! !

!InstvarRemappingTest methodsFor: 'subject code' stamp: 'jmv 8/1/2025 09:38:20'!
m02SourceCode
	^ 'test02AccessMidVariables
 
 	"Med #genStorePopInstVar:. Use two bytes each in Spur."
 	a25 := ''a1''.
 	a26 := 2.
 	a27 := Float pi.
 	a28 := #[11 22 33 44].
 	1 > 0
 		ifTrue: [ a245 := 99 ]
 		ifFalse: [ a245 := 98 ].
 	a246 := ''a1''.
 	a247 := Float pi.
 	a248 := #[11 22 33 44].
 	
 	"Med #genPushInstVar: . Use two bytes each in Spur."
 	self assert: a25 = ''a1''.
 	self assert: a26 = 2.
 	self assert: a27 = Float pi.
 	self assert: a28 = #[11 22 33 44].
 	self assert: a245 = 99.
 	self assert: a246 = ''a1''.
 	self assert: a247 = Float pi.
 	self assert: a248 = #[11 22 33 44].'! !

!InstvarRemappingTest methodsFor: 'subject code' stamp: 'jmv 8/1/2025 10:38:29'!
m03SourceCode
	^ 'test03AccessHighVariables
 	1 > 0
 		ifTrue: [ a255 := 99 ]
 		ifFalse: [ a255 := 98 ].
 	a256 := ''a1''.
 	a257 := Float pi.
 	a258 := #[11 22 33 44].
 	self assert: a255 = 99.
 	self assert: a256 = ''a1''.
 	self assert: a257 = Float pi.
 	self assert: a258 = #[11 22 33 44].'! !

!InstvarRemappingTest methodsFor: 'subject code' stamp: 'jmv 8/1/2025 09:38:29'!
m04SourceCode
	^ 'test04LongJumps
 	1 > 0
 		ifTrue: [ a255 := 99 ]
 		ifFalse: [
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.{''La tota''} print.
 			a255 := 98 ].
 	a256 := ''a1''.
 	a257 := Float pi.
 	a258 := #[11 22 33 44].
 	self assert: a255 = 99.
 	self assert: a256 = ''a1''.
 	self assert: a257 = Float pi.
 	self assert: a258 = #[11 22 33 44].'! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:24:50'!
m01Bytecode
	^
'73 <51> pushConstant: 1
74 <50> pushConstant: 0
75 <63> send: >
76 <C4> jumpFalse: 82
77 <E8 63> pushConstant: 99
79 <F3 04> storeIntoRcvr: 4 "a5"
81 <B3> jumpTo: 86
82 <E8 62> pushConstant: 98
84 <F3 04> storeIntoRcvr: 4 "a5"
86 <D8> pop
87 <20> pushConstant: ''a1''
88 <CD> popIntoRcvr: 5 "a6"
89 <5F> nop
90 <12> pushLit: Float
91 <81> send: pi
92 <CE> popIntoRcvr: 6 "a7"
93 <5F> nop
94 <23> pushConstant: #[11 22 33 44]
95 <CF> popIntoRcvr: 7 "a8"
96 <5F> nop
97 <4C> self
98 <04> pushRcvr: 4 "a5"
99 <5F> nop
100 <E8 63> pushConstant: 99
102 <66> send: =
103 <94> send: assert:
104 <D8> pop
105 <4C> self
106 <05> pushRcvr: 5 "a6"
107 <5F> nop
108 <20> pushConstant: ''a1''
109 <66> send: =
110 <94> send: assert:
111 <D8> pop
112 <4C> self
113 <06> pushRcvr: 6 "a7"
114 <5F> nop
115 <12> pushLit: Float
116 <81> send: pi
117 <66> send: =
118 <94> send: assert:
119 <D8> pop
120 <4C> self
121 <07> pushRcvr: 7 "a8"
122 <5F> nop
123 <23> pushConstant: #[11 22 33 44]
124 <66> send: =
125 <94> send: assert:
126 <D8> pop
127 <58> returnSelf
'.
! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:25:17'!
m01BytecodeLongIvarAcessAndJumps
	^
'73 <51> pushConstant: 1
74 <50> pushConstant: 0
75 <63> send: >
76 <E1 00 EF 0A> jumpFalse: 90
80 <E8 63> pushConstant: 99
82 <E0 00 F3 04> storeIntoRcvr: 4 "a5"
86 <E1 00 ED 06> jumpTo: 96
90 <E8 62> pushConstant: 98
92 <E0 00 F3 04> storeIntoRcvr: 4 "a5"
96 <D8> pop
97 <20> pushConstant: ''a1''
98 <E0 00 CD> popIntoRcvr: 5 "a6"
101 <5F> nop
102 <12> pushLit: Float
103 <81> send: pi
104 <E0 00 CE> popIntoRcvr: 6 "a7"
107 <5F> nop
108 <23> pushConstant: #[11 22 33 44]
109 <E0 00 CF> popIntoRcvr: 7 "a8"
112 <5F> nop
113 <4C> self
114 <E0 00 04> pushRcvr: 4 "a5"
117 <5F> nop
118 <E8 63> pushConstant: 99
120 <66> send: =
121 <94> send: assert:
122 <D8> pop
123 <4C> self
124 <E0 00 05> pushRcvr: 5 "a6"
127 <5F> nop
128 <20> pushConstant: ''a1''
129 <66> send: =
130 <94> send: assert:
131 <D8> pop
132 <4C> self
133 <E0 00 06> pushRcvr: 6 "a7"
136 <5F> nop
137 <12> pushLit: Float
138 <81> send: pi
139 <66> send: =
140 <94> send: assert:
141 <D8> pop
142 <4C> self
143 <E0 00 07> pushRcvr: 7 "a8"
146 <5F> nop
147 <23> pushConstant: #[11 22 33 44]
148 <66> send: =
149 <94> send: assert:
150 <D8> pop
151 <58> returnSelf
'.! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:25:52'!
m02Bytecode
	^
'73 <20> pushConstant: ''a1''
74 <F0 18> popIntoRcvr: 24 "a25"
76 <E8 02> pushConstant: 2
78 <F0 19> popIntoRcvr: 25 "a26"
80 <12> pushLit: Float
81 <81> send: pi
82 <F0 1A> popIntoRcvr: 26 "a27"
84 <23> pushConstant: #[11 22 33 44]
85 <F0 1B> popIntoRcvr: 27 "a28"
87 <51> pushConstant: 1
88 <50> pushConstant: 0
89 <63> send: >
90 <C4> jumpFalse: 96
91 <E8 63> pushConstant: 99
93 <F3 F4> storeIntoRcvr: 244 "a245"
95 <B3> jumpTo: 100
96 <E8 62> pushConstant: 98
98 <F3 F4> storeIntoRcvr: 244 "a245"
100 <D8> pop
101 <20> pushConstant: ''a1''
102 <F0 F5> popIntoRcvr: 245 "a246"
104 <12> pushLit: Float
105 <81> send: pi
106 <F0 F6> popIntoRcvr: 246 "a247"
108 <23> pushConstant: #[11 22 33 44]
109 <F0 F7> popIntoRcvr: 247 "a248"
111 <4C> self
112 <E2 18> pushRcvr: 24 "a25"
114 <20> pushConstant: ''a1''
115 <66> send: =
116 <94> send: assert:
117 <D8> pop
118 <4C> self
119 <E2 19> pushRcvr: 25 "a26"
121 <E8 02> pushConstant: 2
123 <66> send: =
124 <94> send: assert:
125 <D8> pop
126 <4C> self
127 <E2 1A> pushRcvr: 26 "a27"
129 <12> pushLit: Float
130 <81> send: pi
131 <66> send: =
132 <94> send: assert:
133 <D8> pop
134 <4C> self
135 <E2 1B> pushRcvr: 27 "a28"
137 <23> pushConstant: #[11 22 33 44]
138 <66> send: =
139 <94> send: assert:
140 <D8> pop
141 <4C> self
142 <E2 F4> pushRcvr: 244 "a245"
144 <E8 63> pushConstant: 99
146 <66> send: =
147 <94> send: assert:
148 <D8> pop
149 <4C> self
150 <E2 F5> pushRcvr: 245 "a246"
152 <20> pushConstant: ''a1''
153 <66> send: =
154 <94> send: assert:
155 <D8> pop
156 <4C> self
157 <E2 F6> pushRcvr: 246 "a247"
159 <12> pushLit: Float
160 <81> send: pi
161 <66> send: =
162 <94> send: assert:
163 <D8> pop
164 <4C> self
165 <E2 F7> pushRcvr: 247 "a248"
167 <23> pushConstant: #[11 22 33 44]
168 <66> send: =
169 <94> send: assert:
170 <D8> pop
171 <58> returnSelf
'.
! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:26:14'!
m02BytecodeLongIvarAcessAndJumps
	^
'73 <20> pushConstant: ''a1''
74 <E0 00 F0 18> popIntoRcvr: 24 "a25"
78 <E8 02> pushConstant: 2
80 <E0 00 F0 19> popIntoRcvr: 25 "a26"
84 <12> pushLit: Float
85 <81> send: pi
86 <E0 00 F0 1A> popIntoRcvr: 26 "a27"
90 <23> pushConstant: #[11 22 33 44]
91 <E0 00 F0 1B> popIntoRcvr: 27 "a28"
95 <51> pushConstant: 1
96 <50> pushConstant: 0
97 <63> send: >
98 <E1 00 EF 0A> jumpFalse: 112
102 <E8 63> pushConstant: 99
104 <E0 00 F3 F4> storeIntoRcvr: 244 "a245"
108 <E1 00 ED 06> jumpTo: 118
112 <E8 62> pushConstant: 98
114 <E0 00 F3 F4> storeIntoRcvr: 244 "a245"
118 <D8> pop
119 <20> pushConstant: ''a1''
120 <E0 00 F0 F5> popIntoRcvr: 245 "a246"
124 <12> pushLit: Float
125 <81> send: pi
126 <E0 00 F0 F6> popIntoRcvr: 246 "a247"
130 <23> pushConstant: #[11 22 33 44]
131 <E0 00 F0 F7> popIntoRcvr: 247 "a248"
135 <4C> self
136 <E0 00 E2 18> pushRcvr: 24 "a25"
140 <20> pushConstant: ''a1''
141 <66> send: =
142 <94> send: assert:
143 <D8> pop
144 <4C> self
145 <E0 00 E2 19> pushRcvr: 25 "a26"
149 <E8 02> pushConstant: 2
151 <66> send: =
152 <94> send: assert:
153 <D8> pop
154 <4C> self
155 <E0 00 E2 1A> pushRcvr: 26 "a27"
159 <12> pushLit: Float
160 <81> send: pi
161 <66> send: =
162 <94> send: assert:
163 <D8> pop
164 <4C> self
165 <E0 00 E2 1B> pushRcvr: 27 "a28"
169 <23> pushConstant: #[11 22 33 44]
170 <66> send: =
171 <94> send: assert:
172 <D8> pop
173 <4C> self
174 <E0 00 E2 F4> pushRcvr: 244 "a245"
178 <E8 63> pushConstant: 99
180 <66> send: =
181 <94> send: assert:
182 <D8> pop
183 <4C> self
184 <E0 00 E2 F5> pushRcvr: 245 "a246"
188 <20> pushConstant: ''a1''
189 <66> send: =
190 <94> send: assert:
191 <D8> pop
192 <4C> self
193 <E0 00 E2 F6> pushRcvr: 246 "a247"
197 <12> pushLit: Float
198 <81> send: pi
199 <66> send: =
200 <94> send: assert:
201 <D8> pop
202 <4C> self
203 <E0 00 E2 F7> pushRcvr: 247 "a248"
207 <23> pushConstant: #[11 22 33 44]
208 <66> send: =
209 <94> send: assert:
210 <D8> pop
211 <58> returnSelf
'.! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:26:46'!
m03Bytecode
	^
'73 <51> pushConstant: 1
74 <50> pushConstant: 0
75 <63> send: >
76 <C4> jumpFalse: 82
77 <E8 63> pushConstant: 99
79 <F3 FE> storeIntoRcvr: 254 "a255"
81 <B3> jumpTo: 86
82 <E8 62> pushConstant: 98
84 <F3 FE> storeIntoRcvr: 254 "a255"
86 <D8> pop
87 <20> pushConstant: ''a1''
88 <F0 FF> popIntoRcvr: 255 "a256"
90 <12> pushLit: Float
91 <81> send: pi
92 <E0 01 F0 00> popIntoRcvr: 256 "a257"
96 <23> pushConstant: #[11 22 33 44]
97 <E0 01 F0 01> popIntoRcvr: 257 "a258"
101 <4C> self
102 <E2 FE> pushRcvr: 254 "a255"
104 <E8 63> pushConstant: 99
106 <66> send: =
107 <94> send: assert:
108 <D8> pop
109 <4C> self
110 <E2 FF> pushRcvr: 255 "a256"
112 <20> pushConstant: ''a1''
113 <66> send: =
114 <94> send: assert:
115 <D8> pop
116 <4C> self
117 <E0 01 E2 00> pushRcvr: 256 "a257"
121 <12> pushLit: Float
122 <81> send: pi
123 <66> send: =
124 <94> send: assert:
125 <D8> pop
126 <4C> self
127 <E0 01 E2 01> pushRcvr: 257 "a258"
131 <23> pushConstant: #[11 22 33 44]
132 <66> send: =
133 <94> send: assert:
134 <D8> pop
135 <58> returnSelf
'.
! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:27:05'!
m03BytecodeLongIvarAcessAndJumps
	^
'73 <51> pushConstant: 1
74 <50> pushConstant: 0
75 <63> send: >
76 <E1 00 EF 0A> jumpFalse: 90
80 <E8 63> pushConstant: 99
82 <E0 00 F3 FE> storeIntoRcvr: 254 "a255"
86 <E1 00 ED 06> jumpTo: 96
90 <E8 62> pushConstant: 98
92 <E0 00 F3 FE> storeIntoRcvr: 254 "a255"
96 <D8> pop
97 <20> pushConstant: ''a1''
98 <E0 00 F0 FF> popIntoRcvr: 255 "a256"
102 <12> pushLit: Float
103 <81> send: pi
104 <E0 01 F0 00> popIntoRcvr: 256 "a257"
108 <23> pushConstant: #[11 22 33 44]
109 <E0 01 F0 01> popIntoRcvr: 257 "a258"
113 <4C> self
114 <E0 00 E2 FE> pushRcvr: 254 "a255"
118 <E8 63> pushConstant: 99
120 <66> send: =
121 <94> send: assert:
122 <D8> pop
123 <4C> self
124 <E0 00 E2 FF> pushRcvr: 255 "a256"
128 <20> pushConstant: ''a1''
129 <66> send: =
130 <94> send: assert:
131 <D8> pop
132 <4C> self
133 <E0 01 E2 00> pushRcvr: 256 "a257"
137 <12> pushLit: Float
138 <81> send: pi
139 <66> send: =
140 <94> send: assert:
141 <D8> pop
142 <4C> self
143 <E0 01 E2 01> pushRcvr: 257 "a258"
147 <23> pushConstant: #[11 22 33 44]
148 <66> send: =
149 <94> send: assert:
150 <D8> pop
151 <58> returnSelf
'.! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:29:06'!
m04Bytecode
	^
'89 <51> pushConstant: 1
90 <50> pushConstant: 0
91 <63> send: >
92 <C7> jumpFalse: 101
93 <E8 63> pushConstant: 99
95 <F3 FE> storeIntoRcvr: 254 "a255"
97 <E1 01 ED 3F> jumpTo: 420
101 <21> pushConstant: ''La tota''
102 <E7 81> pop 1 into (Array new: 1)
104 <80> send: print
105 <D8> pop
106 <21> pushConstant: ''La tota''
107 <E7 81> pop 1 into (Array new: 1)
109 <80> send: print
110 <D8> pop
111 <21> pushConstant: ''La tota''
112 <E7 81> pop 1 into (Array new: 1)
114 <80> send: print
115 <D8> pop
116 <21> pushConstant: ''La tota''
117 <E7 81> pop 1 into (Array new: 1)
119 <80> send: print
120 <D8> pop
121 <21> pushConstant: ''La tota''
122 <E7 81> pop 1 into (Array new: 1)
124 <80> send: print
125 <D8> pop
126 <21> pushConstant: ''La tota''
127 <E7 81> pop 1 into (Array new: 1)
129 <80> send: print
130 <D8> pop
131 <21> pushConstant: ''La tota''
132 <E7 81> pop 1 into (Array new: 1)
134 <80> send: print
135 <D8> pop
136 <21> pushConstant: ''La tota''
137 <E7 81> pop 1 into (Array new: 1)
139 <80> send: print
140 <D8> pop
141 <21> pushConstant: ''La tota''
142 <E7 81> pop 1 into (Array new: 1)
144 <80> send: print
145 <D8> pop
146 <21> pushConstant: ''La tota''
147 <E7 81> pop 1 into (Array new: 1)
149 <80> send: print
150 <D8> pop
151 <21> pushConstant: ''La tota''
152 <E7 81> pop 1 into (Array new: 1)
154 <80> send: print
155 <D8> pop
156 <21> pushConstant: ''La tota''
157 <E7 81> pop 1 into (Array new: 1)
159 <80> send: print
160 <D8> pop
161 <21> pushConstant: ''La tota''
162 <E7 81> pop 1 into (Array new: 1)
164 <80> send: print
165 <D8> pop
166 <21> pushConstant: ''La tota''
167 <E7 81> pop 1 into (Array new: 1)
169 <80> send: print
170 <D8> pop
171 <21> pushConstant: ''La tota''
172 <E7 81> pop 1 into (Array new: 1)
174 <80> send: print
175 <D8> pop
176 <21> pushConstant: ''La tota''
177 <E7 81> pop 1 into (Array new: 1)
179 <80> send: print
180 <D8> pop
181 <21> pushConstant: ''La tota''
182 <E7 81> pop 1 into (Array new: 1)
184 <80> send: print
185 <D8> pop
186 <21> pushConstant: ''La tota''
187 <E7 81> pop 1 into (Array new: 1)
189 <80> send: print
190 <D8> pop
191 <21> pushConstant: ''La tota''
192 <E7 81> pop 1 into (Array new: 1)
194 <80> send: print
195 <D8> pop
196 <21> pushConstant: ''La tota''
197 <E7 81> pop 1 into (Array new: 1)
199 <80> send: print
200 <D8> pop
201 <21> pushConstant: ''La tota''
202 <E7 81> pop 1 into (Array new: 1)
204 <80> send: print
205 <D8> pop
206 <21> pushConstant: ''La tota''
207 <E7 81> pop 1 into (Array new: 1)
209 <80> send: print
210 <D8> pop
211 <21> pushConstant: ''La tota''
212 <E7 81> pop 1 into (Array new: 1)
214 <80> send: print
215 <D8> pop
216 <21> pushConstant: ''La tota''
217 <E7 81> pop 1 into (Array new: 1)
219 <80> send: print
220 <D8> pop
221 <21> pushConstant: ''La tota''
222 <E7 81> pop 1 into (Array new: 1)
224 <80> send: print
225 <D8> pop
226 <21> pushConstant: ''La tota''
227 <E7 81> pop 1 into (Array new: 1)
229 <80> send: print
230 <D8> pop
231 <21> pushConstant: ''La tota''
232 <E7 81> pop 1 into (Array new: 1)
234 <80> send: print
235 <D8> pop
236 <21> pushConstant: ''La tota''
237 <E7 81> pop 1 into (Array new: 1)
239 <80> send: print
240 <D8> pop
241 <21> pushConstant: ''La tota''
242 <E7 81> pop 1 into (Array new: 1)
244 <80> send: print
245 <D8> pop
246 <21> pushConstant: ''La tota''
247 <E7 81> pop 1 into (Array new: 1)
249 <80> send: print
250 <D8> pop
251 <21> pushConstant: ''La tota''
252 <E7 81> pop 1 into (Array new: 1)
254 <80> send: print
255 <D8> pop
256 <21> pushConstant: ''La tota''
257 <E7 81> pop 1 into (Array new: 1)
259 <80> send: print
260 <D8> pop
261 <21> pushConstant: ''La tota''
262 <E7 81> pop 1 into (Array new: 1)
264 <80> send: print
265 <D8> pop
266 <21> pushConstant: ''La tota''
267 <E7 81> pop 1 into (Array new: 1)
269 <80> send: print
270 <D8> pop
271 <21> pushConstant: ''La tota''
272 <E7 81> pop 1 into (Array new: 1)
274 <80> send: print
275 <D8> pop
276 <21> pushConstant: ''La tota''
277 <E7 81> pop 1 into (Array new: 1)
279 <80> send: print
280 <D8> pop
281 <21> pushConstant: ''La tota''
282 <E7 81> pop 1 into (Array new: 1)
284 <80> send: print
285 <D8> pop
286 <21> pushConstant: ''La tota''
287 <E7 81> pop 1 into (Array new: 1)
289 <80> send: print
290 <D8> pop
291 <21> pushConstant: ''La tota''
292 <E7 81> pop 1 into (Array new: 1)
294 <80> send: print
295 <D8> pop
296 <21> pushConstant: ''La tota''
297 <E7 81> pop 1 into (Array new: 1)
299 <80> send: print
300 <D8> pop
301 <21> pushConstant: ''La tota''
302 <E7 81> pop 1 into (Array new: 1)
304 <80> send: print
305 <D8> pop
306 <21> pushConstant: ''La tota''
307 <E7 81> pop 1 into (Array new: 1)
309 <80> send: print
310 <D8> pop
311 <21> pushConstant: ''La tota''
312 <E7 81> pop 1 into (Array new: 1)
314 <80> send: print
315 <D8> pop
316 <21> pushConstant: ''La tota''
317 <E7 81> pop 1 into (Array new: 1)
319 <80> send: print
320 <D8> pop
321 <21> pushConstant: ''La tota''
322 <E7 81> pop 1 into (Array new: 1)
324 <80> send: print
325 <D8> pop
326 <21> pushConstant: ''La tota''
327 <E7 81> pop 1 into (Array new: 1)
329 <80> send: print
330 <D8> pop
331 <21> pushConstant: ''La tota''
332 <E7 81> pop 1 into (Array new: 1)
334 <80> send: print
335 <D8> pop
336 <21> pushConstant: ''La tota''
337 <E7 81> pop 1 into (Array new: 1)
339 <80> send: print
340 <D8> pop
341 <21> pushConstant: ''La tota''
342 <E7 81> pop 1 into (Array new: 1)
344 <80> send: print
345 <D8> pop
346 <21> pushConstant: ''La tota''
347 <E7 81> pop 1 into (Array new: 1)
349 <80> send: print
350 <D8> pop
351 <21> pushConstant: ''La tota''
352 <E7 81> pop 1 into (Array new: 1)
354 <80> send: print
355 <D8> pop
356 <21> pushConstant: ''La tota''
357 <E7 81> pop 1 into (Array new: 1)
359 <80> send: print
360 <D8> pop
361 <21> pushConstant: ''La tota''
362 <E7 81> pop 1 into (Array new: 1)
364 <80> send: print
365 <D8> pop
366 <21> pushConstant: ''La tota''
367 <E7 81> pop 1 into (Array new: 1)
369 <80> send: print
370 <D8> pop
371 <21> pushConstant: ''La tota''
372 <E7 81> pop 1 into (Array new: 1)
374 <80> send: print
375 <D8> pop
376 <21> pushConstant: ''La tota''
377 <E7 81> pop 1 into (Array new: 1)
379 <80> send: print
380 <D8> pop
381 <21> pushConstant: ''La tota''
382 <E7 81> pop 1 into (Array new: 1)
384 <80> send: print
385 <D8> pop
386 <21> pushConstant: ''La tota''
387 <E7 81> pop 1 into (Array new: 1)
389 <80> send: print
390 <D8> pop
391 <21> pushConstant: ''La tota''
392 <E7 81> pop 1 into (Array new: 1)
394 <80> send: print
395 <D8> pop
396 <21> pushConstant: ''La tota''
397 <E7 81> pop 1 into (Array new: 1)
399 <80> send: print
400 <D8> pop
401 <21> pushConstant: ''La tota''
402 <E7 81> pop 1 into (Array new: 1)
404 <80> send: print
405 <D8> pop
406 <21> pushConstant: ''La tota''
407 <E7 81> pop 1 into (Array new: 1)
409 <80> send: print
410 <D8> pop
411 <21> pushConstant: ''La tota''
412 <E7 81> pop 1 into (Array new: 1)
414 <80> send: print
415 <D8> pop
416 <E8 62> pushConstant: 98
418 <F3 FE> storeIntoRcvr: 254 "a255"
420 <D8> pop
421 <22> pushConstant: ''a1''
422 <F0 FF> popIntoRcvr: 255 "a256"
424 <14> pushLit: Float
425 <83> send: pi
426 <E0 01 F0 00> popIntoRcvr: 256 "a257"
430 <25> pushConstant: #[11 22 33 44]
431 <E0 01 F0 01> popIntoRcvr: 257 "a258"
435 <4C> self
436 <E2 FE> pushRcvr: 254 "a255"
438 <E8 63> pushConstant: 99
440 <66> send: =
441 <96> send: assert:
442 <D8> pop
443 <4C> self
444 <E2 FF> pushRcvr: 255 "a256"
446 <22> pushConstant: ''a1''
447 <66> send: =
448 <96> send: assert:
449 <D8> pop
450 <4C> self
451 <E0 01 E2 00> pushRcvr: 256 "a257"
455 <14> pushLit: Float
456 <83> send: pi
457 <66> send: =
458 <96> send: assert:
459 <D8> pop
460 <4C> self
461 <E0 01 E2 01> pushRcvr: 257 "a258"
465 <25> pushConstant: #[11 22 33 44]
466 <66> send: =
467 <96> send: assert:
468 <D8> pop
469 <58> returnSelf
'! !

!InstvarRemappingTest methodsFor: 'bytecode code' stamp: 'jmv 8/1/2025 10:29:35'!
m04BytecodeLongIvarAcessAndJumps
	^
'89 <51> pushConstant: 1
90 <50> pushConstant: 0
91 <63> send: >
92 <E1 00 EF 0A> jumpFalse: 106
96 <E8 63> pushConstant: 99
98 <E0 00 F3 FE> storeIntoRcvr: 254 "a255"
102 <E1 01 ED 41> jumpTo: 427
106 <21> pushConstant: ''La tota''
107 <E7 81> pop 1 into (Array new: 1)
109 <80> send: print
110 <D8> pop
111 <21> pushConstant: ''La tota''
112 <E7 81> pop 1 into (Array new: 1)
114 <80> send: print
115 <D8> pop
116 <21> pushConstant: ''La tota''
117 <E7 81> pop 1 into (Array new: 1)
119 <80> send: print
120 <D8> pop
121 <21> pushConstant: ''La tota''
122 <E7 81> pop 1 into (Array new: 1)
124 <80> send: print
125 <D8> pop
126 <21> pushConstant: ''La tota''
127 <E7 81> pop 1 into (Array new: 1)
129 <80> send: print
130 <D8> pop
131 <21> pushConstant: ''La tota''
132 <E7 81> pop 1 into (Array new: 1)
134 <80> send: print
135 <D8> pop
136 <21> pushConstant: ''La tota''
137 <E7 81> pop 1 into (Array new: 1)
139 <80> send: print
140 <D8> pop
141 <21> pushConstant: ''La tota''
142 <E7 81> pop 1 into (Array new: 1)
144 <80> send: print
145 <D8> pop
146 <21> pushConstant: ''La tota''
147 <E7 81> pop 1 into (Array new: 1)
149 <80> send: print
150 <D8> pop
151 <21> pushConstant: ''La tota''
152 <E7 81> pop 1 into (Array new: 1)
154 <80> send: print
155 <D8> pop
156 <21> pushConstant: ''La tota''
157 <E7 81> pop 1 into (Array new: 1)
159 <80> send: print
160 <D8> pop
161 <21> pushConstant: ''La tota''
162 <E7 81> pop 1 into (Array new: 1)
164 <80> send: print
165 <D8> pop
166 <21> pushConstant: ''La tota''
167 <E7 81> pop 1 into (Array new: 1)
169 <80> send: print
170 <D8> pop
171 <21> pushConstant: ''La tota''
172 <E7 81> pop 1 into (Array new: 1)
174 <80> send: print
175 <D8> pop
176 <21> pushConstant: ''La tota''
177 <E7 81> pop 1 into (Array new: 1)
179 <80> send: print
180 <D8> pop
181 <21> pushConstant: ''La tota''
182 <E7 81> pop 1 into (Array new: 1)
184 <80> send: print
185 <D8> pop
186 <21> pushConstant: ''La tota''
187 <E7 81> pop 1 into (Array new: 1)
189 <80> send: print
190 <D8> pop
191 <21> pushConstant: ''La tota''
192 <E7 81> pop 1 into (Array new: 1)
194 <80> send: print
195 <D8> pop
196 <21> pushConstant: ''La tota''
197 <E7 81> pop 1 into (Array new: 1)
199 <80> send: print
200 <D8> pop
201 <21> pushConstant: ''La tota''
202 <E7 81> pop 1 into (Array new: 1)
204 <80> send: print
205 <D8> pop
206 <21> pushConstant: ''La tota''
207 <E7 81> pop 1 into (Array new: 1)
209 <80> send: print
210 <D8> pop
211 <21> pushConstant: ''La tota''
212 <E7 81> pop 1 into (Array new: 1)
214 <80> send: print
215 <D8> pop
216 <21> pushConstant: ''La tota''
217 <E7 81> pop 1 into (Array new: 1)
219 <80> send: print
220 <D8> pop
221 <21> pushConstant: ''La tota''
222 <E7 81> pop 1 into (Array new: 1)
224 <80> send: print
225 <D8> pop
226 <21> pushConstant: ''La tota''
227 <E7 81> pop 1 into (Array new: 1)
229 <80> send: print
230 <D8> pop
231 <21> pushConstant: ''La tota''
232 <E7 81> pop 1 into (Array new: 1)
234 <80> send: print
235 <D8> pop
236 <21> pushConstant: ''La tota''
237 <E7 81> pop 1 into (Array new: 1)
239 <80> send: print
240 <D8> pop
241 <21> pushConstant: ''La tota''
242 <E7 81> pop 1 into (Array new: 1)
244 <80> send: print
245 <D8> pop
246 <21> pushConstant: ''La tota''
247 <E7 81> pop 1 into (Array new: 1)
249 <80> send: print
250 <D8> pop
251 <21> pushConstant: ''La tota''
252 <E7 81> pop 1 into (Array new: 1)
254 <80> send: print
255 <D8> pop
256 <21> pushConstant: ''La tota''
257 <E7 81> pop 1 into (Array new: 1)
259 <80> send: print
260 <D8> pop
261 <21> pushConstant: ''La tota''
262 <E7 81> pop 1 into (Array new: 1)
264 <80> send: print
265 <D8> pop
266 <21> pushConstant: ''La tota''
267 <E7 81> pop 1 into (Array new: 1)
269 <80> send: print
270 <D8> pop
271 <21> pushConstant: ''La tota''
272 <E7 81> pop 1 into (Array new: 1)
274 <80> send: print
275 <D8> pop
276 <21> pushConstant: ''La tota''
277 <E7 81> pop 1 into (Array new: 1)
279 <80> send: print
280 <D8> pop
281 <21> pushConstant: ''La tota''
282 <E7 81> pop 1 into (Array new: 1)
284 <80> send: print
285 <D8> pop
286 <21> pushConstant: ''La tota''
287 <E7 81> pop 1 into (Array new: 1)
289 <80> send: print
290 <D8> pop
291 <21> pushConstant: ''La tota''
292 <E7 81> pop 1 into (Array new: 1)
294 <80> send: print
295 <D8> pop
296 <21> pushConstant: ''La tota''
297 <E7 81> pop 1 into (Array new: 1)
299 <80> send: print
300 <D8> pop
301 <21> pushConstant: ''La tota''
302 <E7 81> pop 1 into (Array new: 1)
304 <80> send: print
305 <D8> pop
306 <21> pushConstant: ''La tota''
307 <E7 81> pop 1 into (Array new: 1)
309 <80> send: print
310 <D8> pop
311 <21> pushConstant: ''La tota''
312 <E7 81> pop 1 into (Array new: 1)
314 <80> send: print
315 <D8> pop
316 <21> pushConstant: ''La tota''
317 <E7 81> pop 1 into (Array new: 1)
319 <80> send: print
320 <D8> pop
321 <21> pushConstant: ''La tota''
322 <E7 81> pop 1 into (Array new: 1)
324 <80> send: print
325 <D8> pop
326 <21> pushConstant: ''La tota''
327 <E7 81> pop 1 into (Array new: 1)
329 <80> send: print
330 <D8> pop
331 <21> pushConstant: ''La tota''
332 <E7 81> pop 1 into (Array new: 1)
334 <80> send: print
335 <D8> pop
336 <21> pushConstant: ''La tota''
337 <E7 81> pop 1 into (Array new: 1)
339 <80> send: print
340 <D8> pop
341 <21> pushConstant: ''La tota''
342 <E7 81> pop 1 into (Array new: 1)
344 <80> send: print
345 <D8> pop
346 <21> pushConstant: ''La tota''
347 <E7 81> pop 1 into (Array new: 1)
349 <80> send: print
350 <D8> pop
351 <21> pushConstant: ''La tota''
352 <E7 81> pop 1 into (Array new: 1)
354 <80> send: print
355 <D8> pop
356 <21> pushConstant: ''La tota''
357 <E7 81> pop 1 into (Array new: 1)
359 <80> send: print
360 <D8> pop
361 <21> pushConstant: ''La tota''
362 <E7 81> pop 1 into (Array new: 1)
364 <80> send: print
365 <D8> pop
366 <21> pushConstant: ''La tota''
367 <E7 81> pop 1 into (Array new: 1)
369 <80> send: print
370 <D8> pop
371 <21> pushConstant: ''La tota''
372 <E7 81> pop 1 into (Array new: 1)
374 <80> send: print
375 <D8> pop
376 <21> pushConstant: ''La tota''
377 <E7 81> pop 1 into (Array new: 1)
379 <80> send: print
380 <D8> pop
381 <21> pushConstant: ''La tota''
382 <E7 81> pop 1 into (Array new: 1)
384 <80> send: print
385 <D8> pop
386 <21> pushConstant: ''La tota''
387 <E7 81> pop 1 into (Array new: 1)
389 <80> send: print
390 <D8> pop
391 <21> pushConstant: ''La tota''
392 <E7 81> pop 1 into (Array new: 1)
394 <80> send: print
395 <D8> pop
396 <21> pushConstant: ''La tota''
397 <E7 81> pop 1 into (Array new: 1)
399 <80> send: print
400 <D8> pop
401 <21> pushConstant: ''La tota''
402 <E7 81> pop 1 into (Array new: 1)
404 <80> send: print
405 <D8> pop
406 <21> pushConstant: ''La tota''
407 <E7 81> pop 1 into (Array new: 1)
409 <80> send: print
410 <D8> pop
411 <21> pushConstant: ''La tota''
412 <E7 81> pop 1 into (Array new: 1)
414 <80> send: print
415 <D8> pop
416 <21> pushConstant: ''La tota''
417 <E7 81> pop 1 into (Array new: 1)
419 <80> send: print
420 <D8> pop
421 <E8 62> pushConstant: 98
423 <E0 00 F3 FE> storeIntoRcvr: 254 "a255"
427 <D8> pop
428 <22> pushConstant: ''a1''
429 <E0 00 F0 FF> popIntoRcvr: 255 "a256"
433 <14> pushLit: Float
434 <83> send: pi
435 <E0 01 F0 00> popIntoRcvr: 256 "a257"
439 <25> pushConstant: #[11 22 33 44]
440 <E0 01 F0 01> popIntoRcvr: 257 "a258"
444 <4C> self
445 <E0 00 E2 FE> pushRcvr: 254 "a255"
449 <E8 63> pushConstant: 99
451 <66> send: =
452 <96> send: assert:
453 <D8> pop
454 <4C> self
455 <E0 00 E2 FF> pushRcvr: 255 "a256"
459 <22> pushConstant: ''a1''
460 <66> send: =
461 <96> send: assert:
462 <D8> pop
463 <4C> self
464 <E0 01 E2 00> pushRcvr: 256 "a257"
468 <14> pushLit: Float
469 <83> send: pi
470 <66> send: =
471 <96> send: assert:
472 <D8> pop
473 <4C> self
474 <E0 01 E2 01> pushRcvr: 257 "a258"
478 <25> pushConstant: #[11 22 33 44]
479 <66> send: =
480 <96> send: assert:
481 <D8> pop
482 <58> returnSelf
'! !

!InstvarRemappingTest methodsFor: 'failing' stamp: 'jmv 8/1/2025 10:56:35'!
expectedFailures
	"Answer an array o selectors that are expected to fail.
	Usually this is done for tests that document incorrect behavior for which there is not yet a proper fix available."
	"Only for Sista!!
	Also see #setUp"

	^CompiledMethod preferredBytecodeSetEncoderClass name == #EncoderForSistaV1
		ifTrue: [#()]
		ifFalse: [#(testInstVarRemapping01 testInstVarRemapping02 testInstVarRemapping03 testInstVarRemapping04 testInstVarRemapping05 testInstVarRemapping06 testLongInstVarAcessConversion01 testLongInstVarAcessConversion02 testLongInstVarAcessConversion03 testLongInstVarAcessConversion04)]! !

!Compiler class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:40'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 15 ].
	^ super new parser: self parserClass new.! !

!Scanner class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:43'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	^self basicNew initScanner! !

!ParseNode class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:45'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	LdInstType ifNil: [
		ParseNode initClassCachedState.
		VariableNode initClassCachedState ].
	^super new! !

!MessageNode class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:48'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	MacroSelectors ifNil: [ self initClassCachedState ].
	^super new! !

!UnhandledError methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:55:48'!
devDefaultAction

	LogCompilerUse := false.
	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.! !
DclTest initialize!
