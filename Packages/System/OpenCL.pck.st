'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:18:46 pm'!
'Description '!
!provides: 'OpenCL' 1 50!
!requires: 'FFI' 1 nil nil!
SystemOrganization addCategory: 'OpenCL-Constants'!
SystemOrganization addCategory: 'OpenCL'!
SystemOrganization addCategory: 'OpenCL-Examples'!
SystemOrganization addCategory: 'OpenCL-FFI'!
SystemOrganization addCategory: 'OpenCL-Plugin'!


!classDefinition: #OpenCLConstants category: 'OpenCL-Constants'!
SharedPool subclass: #OpenCLConstants
	instanceVariableNames: ''
	classVariableNames: 'CL_A CL_ABGR CL_ADDRESS_CLAMP CL_ADDRESS_CLAMP_TO_EDGE CL_ADDRESS_MIRRORED_REPEAT CL_ADDRESS_NONE CL_ADDRESS_REPEAT CL_ARGB CL_BGRA CL_BLOCKING CL_BUFFER_CREATE_TYPE_REGION CL_BUILD_ERROR CL_BUILD_IN_PROGRESS CL_BUILD_NONE CL_BUILD_PROGRAM_FAILURE CL_BUILD_SUCCESS CL_COMMAND_ACQUIRE_GL_OBJECTS CL_COMMAND_BARRIER CL_COMMAND_COPY_BUFFER CL_COMMAND_COPY_BUFFER_RECT CL_COMMAND_COPY_BUFFER_TO_IMAGE CL_COMMAND_COPY_IMAGE CL_COMMAND_COPY_IMAGE_TO_BUFFER CL_COMMAND_FILL_BUFFER CL_COMMAND_FILL_IMAGE CL_COMMAND_MAP_BUFFER CL_COMMAND_MAP_IMAGE CL_COMMAND_MARKER CL_COMMAND_MIGRATE_MEM_OBJECTS CL_COMMAND_NATIVE_KERNEL CL_COMMAND_NDRANGE_KERNEL CL_COMMAND_READ_BUFFER CL_COMMAND_READ_BUFFER_RECT CL_COMMAND_READ_IMAGE CL_COMMAND_RELEASE_GL_OBJECTS CL_COMMAND_SVM_FREE CL_COMMAND_SVM_MAP CL_COMMAND_SVM_MEMCPY CL_COMMAND_SVM_MEMFILL CL_COMMAND_SVM_UNMAP CL_COMMAND_TASK CL_COMMAND_UNMAP_MEM_OBJECT CL_COMMAND_USER CL_COMMAND_WRITE_BUFFER CL_COMMAND_WRITE_BUFFER_RECT CL_COMMAND_WRITE_IMAGE CL_COMPILER_NOT_AVAILABLE CL_COMPILE_PROGRAM_FAILURE CL_COMPLETE CL_CONTEXT_DEVICES CL_CONTEXT_INTEROP_USER_SYNC CL_CONTEXT_NUM_DEVICES CL_CONTEXT_PLATFORM CL_CONTEXT_PROPERTIES CL_CONTEXT_REFERENCE_COUNT CL_DEPTH CL_DEPTH_STENCIL CL_DEVICE_ADDRESS_BITS CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE CL_DEVICE_AFFINITY_DOMAIN_NUMA CL_DEVICE_AVAILABLE CL_DEVICE_BUILT_IN_KERNELS CL_DEVICE_COMPILER_AVAILABLE CL_DEVICE_DOUBLE_FP_CONFIG CL_DEVICE_ENDIAN_LITTLE CL_DEVICE_ERROR_CORRECTION_SUPPORT CL_DEVICE_EXECUTION_CAPABILITIES CL_DEVICE_EXTENSIONS CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE CL_DEVICE_GLOBAL_MEM_CACHE_SIZE CL_DEVICE_GLOBAL_MEM_CACHE_TYPE CL_DEVICE_GLOBAL_MEM_SIZE CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE CL_DEVICE_HOST_UNIFIED_MEMORY CL_DEVICE_IMAGE2D_MAX_HEIGHT CL_DEVICE_IMAGE2D_MAX_WIDTH CL_DEVICE_IMAGE3D_MAX_DEPTH CL_DEVICE_IMAGE3D_MAX_HEIGHT CL_DEVICE_IMAGE3D_MAX_WIDTH CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT CL_DEVICE_IMAGE_MAX_ARRAY_SIZE CL_DEVICE_IMAGE_MAX_BUFFER_SIZE CL_DEVICE_IMAGE_PITCH_ALIGNMENT CL_DEVICE_IMAGE_SUPPORT CL_DEVICE_LINKER_AVAILABLE CL_DEVICE_LOCAL_MEM_SIZE CL_DEVICE_LOCAL_MEM_TYPE CL_DEVICE_MAX_CLOCK_FREQUENCY CL_DEVICE_MAX_COMPUTE_UNITS CL_DEVICE_MAX_CONSTANT_ARGS CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE CL_DEVICE_MAX_MEM_ALLOC_SIZE CL_DEVICE_MAX_ON_DEVICE_EVENTS CL_DEVICE_MAX_ON_DEVICE_QUEUES CL_DEVICE_MAX_PARAMETER_SIZE CL_DEVICE_MAX_PIPE_ARGS CL_DEVICE_MAX_READ_IMAGE_ARGS CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS CL_DEVICE_MAX_SAMPLERS CL_DEVICE_MAX_WORK_GROUP_SIZE CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS CL_DEVICE_MAX_WORK_ITEM_SIZES CL_DEVICE_MAX_WRITE_IMAGE_ARGS CL_DEVICE_MEM_BASE_ADDR_ALIGN CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE CL_DEVICE_NAME CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF CL_DEVICE_NATIVE_VECTOR_WIDTH_INT CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT CL_DEVICE_NOT_AVAILABLE CL_DEVICE_NOT_FOUND CL_DEVICE_OPENCL_C_VERSION CL_DEVICE_PARENT_DEVICE CL_DEVICE_PARTITION_AFFINITY_DOMAIN CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN CL_DEVICE_PARTITION_BY_COUNTS CL_DEVICE_PARTITION_BY_COUNTS_LIST_END CL_DEVICE_PARTITION_EQUALLY CL_DEVICE_PARTITION_FAILED CL_DEVICE_PARTITION_MAX_SUB_DEVICES CL_DEVICE_PARTITION_PROPERTIES CL_DEVICE_PARTITION_TYPE CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS CL_DEVICE_PIPE_MAX_PACKET_SIZE CL_DEVICE_PLATFORM CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT CL_DEVICE_PREFERRED_INTEROP_USER_SYNC CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT CL_DEVICE_PRINTF_BUFFER_SIZE CL_DEVICE_PROFILE CL_DEVICE_PROFILING_TIMER_RESOLUTION CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES CL_DEVICE_QUEUE_ON_HOST_PROPERTIES CL_DEVICE_QUEUE_PROPERTIES CL_DEVICE_REFERENCE_COUNT CL_DEVICE_SINGLE_FP_CONFIG CL_DEVICE_SVM_ATOMICS CL_DEVICE_SVM_CAPABILITIES CL_DEVICE_SVM_COARSE_GRAIN_BUFFER CL_DEVICE_SVM_FINE_GRAIN_BUFFER CL_DEVICE_SVM_FINE_GRAIN_SYSTEM CL_DEVICE_TYPE CL_DEVICE_TYPE_ACCELERATOR CL_DEVICE_TYPE_ALL CL_DEVICE_TYPE_CPU CL_DEVICE_TYPE_CUSTOM CL_DEVICE_TYPE_DEFAULT CL_DEVICE_TYPE_GPU CL_DEVICE_VENDOR CL_DEVICE_VENDOR_ID CL_DEVICE_VERSION CL_DRIVER_VERSION CL_EVENT_COMMAND_EXECUTION_STATUS CL_EVENT_COMMAND_QUEUE CL_EVENT_COMMAND_TYPE CL_EVENT_CONTEXT CL_EVENT_REFERENCE_COUNT CL_EXEC_KERNEL CL_EXEC_NATIVE_KERNEL CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST CL_FALSE CL_FILTER_LINEAR CL_FILTER_NEAREST CL_FLOAT CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT CL_FP_DENORM CL_FP_FMA CL_FP_INF_NAN CL_FP_ROUND_TO_INF CL_FP_ROUND_TO_NEAREST CL_FP_ROUND_TO_ZERO CL_FP_SOFT_FLOAT CL_GLOBAL CL_HALF_FLOAT CL_IMAGE_ARRAY_SIZE CL_IMAGE_BUFFER CL_IMAGE_DEPTH CL_IMAGE_ELEMENT_SIZE CL_IMAGE_FORMAT CL_IMAGE_FORMAT_MISMATCH CL_IMAGE_FORMAT_NOT_SUPPORTED CL_IMAGE_HEIGHT CL_IMAGE_NUM_MIP_LEVELS CL_IMAGE_NUM_SAMPLES CL_IMAGE_ROW_PITCH CL_IMAGE_SLICE_PITCH CL_IMAGE_WIDTH CL_INTENSITY CL_INVALID_ARG_INDEX CL_INVALID_ARG_SIZE CL_INVALID_ARG_VALUE CL_INVALID_BINARY CL_INVALID_BUFFER_SIZE CL_INVALID_BUILD_OPTIONS CL_INVALID_COMMAND_QUEUE CL_INVALID_COMPILER_OPTIONS CL_INVALID_CONTEXT CL_INVALID_DEVICE CL_INVALID_DEVICE_PARTITION_COUNT CL_INVALID_DEVICE_QUEUE CL_INVALID_DEVICE_TYPE CL_INVALID_EVENT CL_INVALID_EVENT_WAIT_LIST CL_INVALID_GLOBAL_OFFSET CL_INVALID_GLOBAL_WORK_SIZE CL_INVALID_GL_OBJECT CL_INVALID_HOST_PTR CL_INVALID_IMAGE_DESCRIPTOR CL_INVALID_IMAGE_FORMAT_DESCRIPTOR CL_INVALID_IMAGE_SIZE CL_INVALID_KERNEL CL_INVALID_KERNEL_ARGS CL_INVALID_KERNEL_DEFINITION CL_INVALID_KERNEL_NAME CL_INVALID_LINKER_OPTIONS CL_INVALID_MEM_OBJECT CL_INVALID_MIP_LEVEL CL_INVALID_OPERATION CL_INVALID_PIPE_SIZE CL_INVALID_PLATFORM CL_INVALID_PROGRAM CL_INVALID_PROGRAM_EXECUTABLE CL_INVALID_PROPERTY CL_INVALID_QUEUE_PROPERTIES CL_INVALID_SAMPLER CL_INVALID_VALUE CL_INVALID_WORK_DIMENSION CL_INVALID_WORK_GROUP_SIZE CL_INVALID_WORK_ITEM_SIZE CL_KERNEL_ARG_ACCESS_NONE CL_KERNEL_ARG_ACCESS_QUALIFIER CL_KERNEL_ARG_ACCESS_READ_ONLY CL_KERNEL_ARG_ACCESS_READ_WRITE CL_KERNEL_ARG_ACCESS_WRITE_ONLY CL_KERNEL_ARG_ADDRESS_CONSTANT CL_KERNEL_ARG_ADDRESS_GLOBAL CL_KERNEL_ARG_ADDRESS_LOCAL CL_KERNEL_ARG_ADDRESS_PRIVATE CL_KERNEL_ARG_ADDRESS_QUALIFIER CL_KERNEL_ARG_INFO_NOT_AVAILABLE CL_KERNEL_ARG_NAME CL_KERNEL_ARG_TYPE_CONST CL_KERNEL_ARG_TYPE_NAME CL_KERNEL_ARG_TYPE_NONE CL_KERNEL_ARG_TYPE_PIPE CL_KERNEL_ARG_TYPE_QUALIFIER CL_KERNEL_ARG_TYPE_RESTRICT CL_KERNEL_ARG_TYPE_VOLATILE CL_KERNEL_ATTRIBUTES CL_KERNEL_COMPILE_WORK_GROUP_SIZE CL_KERNEL_CONTEXT CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM CL_KERNEL_EXEC_INFO_SVM_PTRS CL_KERNEL_FUNCTION_NAME CL_KERNEL_GLOBAL_WORK_SIZE CL_KERNEL_LOCAL_MEM_SIZE CL_KERNEL_NUM_ARGS CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE CL_KERNEL_PRIVATE_MEM_SIZE CL_KERNEL_PROGRAM CL_KERNEL_REFERENCE_COUNT CL_KERNEL_WORK_GROUP_SIZE CL_LINKER_NOT_AVAILABLE CL_LINK_PROGRAM_FAILURE CL_LOCAL CL_LUMINANCE CL_MAP_FAILURE CL_MAP_READ CL_MAP_WRITE CL_MAP_WRITE_INVALIDATE_REGION CL_MEM_ALLOC_HOST_PTR CL_MEM_ASSOCIATED_MEMOBJECT CL_MEM_CONTEXT CL_MEM_COPY_HOST_PTR CL_MEM_COPY_OVERLAP CL_MEM_FLAGS CL_MEM_HOST_NO_ACCESS CL_MEM_HOST_PTR CL_MEM_HOST_READ_ONLY CL_MEM_HOST_WRITE_ONLY CL_MEM_KERNEL_READ_AND_WRITE CL_MEM_MAP_COUNT CL_MEM_OBJECT_ALLOCATION_FAILURE CL_MEM_OBJECT_BUFFER CL_MEM_OBJECT_IMAGE1D CL_MEM_OBJECT_IMAGE1D_ARRAY CL_MEM_OBJECT_IMAGE1D_BUFFER CL_MEM_OBJECT_IMAGE2D CL_MEM_OBJECT_IMAGE2D_ARRAY CL_MEM_OBJECT_IMAGE3D CL_MEM_OBJECT_PIPE CL_MEM_OFFSET CL_MEM_READ_ONLY CL_MEM_READ_WRITE CL_MEM_REFERENCE_COUNT CL_MEM_SIZE CL_MEM_SVM_ATOMICS CL_MEM_SVM_FINE_GRAIN_BUFFER CL_MEM_TYPE CL_MEM_USES_SVM_POINTER CL_MEM_USE_HOST_PTR CL_MEM_WRITE_ONLY CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED CL_MIGRATE_MEM_OBJECT_HOST CL_MISALIGNED_SUB_BUFFER_OFFSET CL_NONE CL_NON_BLOCKING CL_OUT_OF_HOST_MEMORY CL_OUT_OF_RESOURCES CL_PIPE_MAX_PACKETS CL_PIPE_PACKET_SIZE CL_PLATFORM_EXTENSIONS CL_PLATFORM_NAME CL_PLATFORM_PROFILE CL_PLATFORM_VENDOR CL_PLATFORM_VERSION CL_PROFILING_COMMAND_COMPLETE CL_PROFILING_COMMAND_END CL_PROFILING_COMMAND_QUEUED CL_PROFILING_COMMAND_START CL_PROFILING_COMMAND_SUBMIT CL_PROFILING_INFO_NOT_AVAILABLE CL_PROGRAM_BINARIES CL_PROGRAM_BINARY_SIZES CL_PROGRAM_BINARY_TYPE CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT CL_PROGRAM_BINARY_TYPE_EXECUTABLE CL_PROGRAM_BINARY_TYPE_LIBRARY CL_PROGRAM_BINARY_TYPE_NONE CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE CL_PROGRAM_BUILD_LOG CL_PROGRAM_BUILD_OPTIONS CL_PROGRAM_BUILD_STATUS CL_PROGRAM_CONTEXT CL_PROGRAM_DEVICES CL_PROGRAM_KERNEL_NAMES CL_PROGRAM_NUM_DEVICES CL_PROGRAM_NUM_KERNELS CL_PROGRAM_REFERENCE_COUNT CL_PROGRAM_SOURCE CL_QUEUED CL_QUEUE_CONTEXT CL_QUEUE_DEVICE CL_QUEUE_ON_DEVICE CL_QUEUE_ON_DEVICE_DEFAULT CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE CL_QUEUE_PROFILING_ENABLE CL_QUEUE_PROPERTIES CL_QUEUE_REFERENCE_COUNT CL_QUEUE_SIZE CL_R CL_RA CL_READ_ONLY_CACHE CL_READ_WRITE_CACHE CL_RG CL_RGB CL_RGBA CL_RGBx CL_RGx CL_RUNNING CL_Rx CL_SAMPLER_ADDRESSING_MODE CL_SAMPLER_CONTEXT CL_SAMPLER_FILTER_MODE CL_SAMPLER_LOD_MAX CL_SAMPLER_LOD_MIN CL_SAMPLER_MIP_FILTER_MODE CL_SAMPLER_NORMALIZED_COORDS CL_SAMPLER_REFERENCE_COUNT CL_SIGNED_INT16 CL_SIGNED_INT32 CL_SIGNED_INT8 CL_SNORM_INT16 CL_SNORM_INT8 CL_SUBMITTED CL_SUCCESS CL_TRUE CL_UNORM_INT16 CL_UNORM_INT24 CL_UNORM_INT8 CL_UNORM_INT_101010 CL_UNORM_SHORT_555 CL_UNORM_SHORT_565 CL_UNSIGNED_INT16 CL_UNSIGNED_INT32 CL_UNSIGNED_INT8 CL_VERSION_1_0 CL_VERSION_1_1 CL_VERSION_1_2 CL_VERSION_2_0 CL_sBGRA CL_sRGB CL_sRGBA CL_sRGBx'
	poolDictionaries: ''
	category: 'OpenCL-Constants'!
!classDefinition: 'OpenCLConstants class' category: 'OpenCL-Constants'!
OpenCLConstants class
	instanceVariableNames: ''!

!classDefinition: #OpenCLError category: 'OpenCL'!
Error subclass: #OpenCLError
	instanceVariableNames: 'errorCode'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL'!
!classDefinition: 'OpenCLError class' category: 'OpenCL'!
OpenCLError class
	instanceVariableNames: ''!

!classDefinition: #OpenCLJuliaSetMorph category: 'OpenCL-Examples'!
ImageMorph subclass: #OpenCLJuliaSetMorph
	instanceVariableNames: 'julia'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLJuliaSetMorph class' category: 'OpenCL-Examples'!
OpenCLJuliaSetMorph class
	instanceVariableNames: ''!

!classDefinition: #CLBufferRegionStruct category: 'OpenCL-FFI'!
ExternalStructure subclass: #CLBufferRegionStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-FFI'!
!classDefinition: 'CLBufferRegionStruct class' category: 'OpenCL-FFI'!
CLBufferRegionStruct class
	instanceVariableNames: ''!

!classDefinition: #CLImageDescStruct category: 'OpenCL-FFI'!
ExternalStructure subclass: #CLImageDescStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-FFI'!
!classDefinition: 'CLImageDescStruct class' category: 'OpenCL-FFI'!
CLImageDescStruct class
	instanceVariableNames: ''!

!classDefinition: #CLImageFormatStruct category: 'OpenCL-FFI'!
ExternalStructure subclass: #CLImageFormatStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-FFI'!
!classDefinition: 'CLImageFormatStruct class' category: 'OpenCL-FFI'!
CLImageFormatStruct class
	instanceVariableNames: ''!

!classDefinition: #OpenCLOpaqueHandle category: 'OpenCL-FFI'!
ExternalStructure subclass: #OpenCLOpaqueHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'OpenCLOpaqueHandle class' category: 'OpenCL-FFI'!
OpenCLOpaqueHandle class
	instanceVariableNames: ''!

!classDefinition: #CLCommandQueue category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLCommandQueue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLCommandQueue class' category: 'OpenCL-FFI'!
CLCommandQueue class
	instanceVariableNames: ''!

!classDefinition: #CLContext category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLContext
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLContext class' category: 'OpenCL-FFI'!
CLContext class
	instanceVariableNames: ''!

!classDefinition: #CLDeviceID category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLDeviceID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLDeviceID class' category: 'OpenCL-FFI'!
CLDeviceID class
	instanceVariableNames: ''!

!classDefinition: #CLEvent category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLEvent class' category: 'OpenCL-FFI'!
CLEvent class
	instanceVariableNames: ''!

!classDefinition: #CLKernel category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLKernel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLKernel class' category: 'OpenCL-FFI'!
CLKernel class
	instanceVariableNames: ''!

!classDefinition: #CLMem category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLMem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLMem class' category: 'OpenCL-FFI'!
CLMem class
	instanceVariableNames: ''!

!classDefinition: #CLPlatformID category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLPlatformID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLPlatformID class' category: 'OpenCL-FFI'!
CLPlatformID class
	instanceVariableNames: ''!

!classDefinition: #CLProgram category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLProgram
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLProgram class' category: 'OpenCL-FFI'!
CLProgram class
	instanceVariableNames: ''!

!classDefinition: #CLSampler category: 'OpenCL-FFI'!
OpenCLOpaqueHandle subclass: #CLSampler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-FFI'!
!classDefinition: 'CLSampler class' category: 'OpenCL-FFI'!
CLSampler class
	instanceVariableNames: ''!

!classDefinition: #OpenCLObject category: 'OpenCL'!
Object subclass: #OpenCLObject
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL'!
!classDefinition: 'OpenCLObject class' category: 'OpenCL'!
OpenCLObject class
	instanceVariableNames: ''!

!classDefinition: #OpenCLDevice category: 'OpenCL'!
OpenCLObject subclass: #OpenCLDevice
	instanceVariableNames: 'platform extensions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLDevice class' category: 'OpenCL'!
OpenCLDevice class
	instanceVariableNames: ''!

!classDefinition: #OpenCLPlatform category: 'OpenCL'!
OpenCLObject subclass: #OpenCLPlatform
	instanceVariableNames: 'profile version name vendor extensions'
	classVariableNames: 'PreferredDevice'
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLPlatform class' category: 'OpenCL'!
OpenCLPlatform class
	instanceVariableNames: ''!

!classDefinition: #OpenCLRefCountedObject category: 'OpenCL'!
OpenCLObject subclass: #OpenCLRefCountedObject
	instanceVariableNames: 'refCounter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLRefCountedObject class' category: 'OpenCL'!
OpenCLRefCountedObject class
	instanceVariableNames: ''!

!classDefinition: #OpenCLContext category: 'OpenCL'!
OpenCLRefCountedObject subclass: #OpenCLContext
	instanceVariableNames: 'devices'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLContext class' category: 'OpenCL'!
OpenCLContext class
	instanceVariableNames: ''!

!classDefinition: #OpenCLContextObject category: 'OpenCL'!
OpenCLRefCountedObject subclass: #OpenCLContextObject
	instanceVariableNames: 'context'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLContextObject class' category: 'OpenCL'!
OpenCLContextObject class
	instanceVariableNames: ''!

!classDefinition: #OpenCLBuffer category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLBuffer
	instanceVariableNames: 'byteSize smalltalkObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLBuffer class' category: 'OpenCL'!
OpenCLBuffer class
	instanceVariableNames: ''!

!classDefinition: #OpenCLCommandQueue category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLCommandQueue
	instanceVariableNames: 'device isInOrder isProfiling'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLCommandQueue class' category: 'OpenCL'!
OpenCLCommandQueue class
	instanceVariableNames: ''!

!classDefinition: #OpenCLImage2D category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLImage2D
	instanceVariableNames: 'format width height rowPitch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLImage2D class' category: 'OpenCL'!
OpenCLImage2D class
	instanceVariableNames: ''!

!classDefinition: #OpenCLImage3D category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLImage3D
	instanceVariableNames: 'format width height depth rowPitch slicePitch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLImage3D class' category: 'OpenCL'!
OpenCLImage3D class
	instanceVariableNames: ''!

!classDefinition: #OpenCLProgram category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLProgram
	instanceVariableNames: 'sourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLProgram class' category: 'OpenCL'!
OpenCLProgram class
	instanceVariableNames: ''!

!classDefinition: #OpenCLSampler category: 'OpenCL'!
OpenCLContextObject subclass: #OpenCLSampler
	instanceVariableNames: 'useNormalizedCoords addressingMode filterMode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLSampler class' category: 'OpenCL'!
OpenCLSampler class
	instanceVariableNames: ''!

!classDefinition: #OpenCLEvent category: 'OpenCL'!
OpenCLRefCountedObject subclass: #OpenCLEvent
	instanceVariableNames: 'commandQueue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLEvent class' category: 'OpenCL'!
OpenCLEvent class
	instanceVariableNames: ''!

!classDefinition: #OpenCLMapEvent category: 'OpenCL'!
OpenCLEvent subclass: #OpenCLMapEvent
	instanceVariableNames: 'pointer rowPitch slicePitch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLMapEvent class' category: 'OpenCL'!
OpenCLMapEvent class
	instanceVariableNames: ''!

!classDefinition: #OpenCLKernel category: 'OpenCL'!
OpenCLRefCountedObject subclass: #OpenCLKernel
	instanceVariableNames: 'program'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLKernel class' category: 'OpenCL'!
OpenCLKernel class
	instanceVariableNames: ''!

!classDefinition: #OpenCLPrimitiveInterface category: 'OpenCL'!
Object subclass: #OpenCLPrimitiveInterface
	instanceVariableNames: ''
	classVariableNames: 'ActiveSubclass FinalizationRegistry'
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL'!
!classDefinition: 'OpenCLPrimitiveInterface class' category: 'OpenCL'!
OpenCLPrimitiveInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCL64BitsPrimitiveInterface category: 'OpenCL'!
OpenCLPrimitiveInterface subclass: #OpenCL64BitsPrimitiveInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCL64BitsPrimitiveInterface class' category: 'OpenCL'!
OpenCL64BitsPrimitiveInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLMacOSInterface category: 'OpenCL'!
OpenCL64BitsPrimitiveInterface subclass: #OpenCLMacOSInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLMacOSInterface class' category: 'OpenCL'!
OpenCLMacOSInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLUnixInterface category: 'OpenCL'!
OpenCL64BitsPrimitiveInterface subclass: #OpenCLUnixInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLUnixInterface class' category: 'OpenCL'!
OpenCLUnixInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLWindowsInterface category: 'OpenCL'!
OpenCL64BitsPrimitiveInterface subclass: #OpenCLWindowsInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLWindowsInterface class' category: 'OpenCL'!
OpenCLWindowsInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLMacOS32BitsInterface category: 'OpenCL'!
OpenCLPrimitiveInterface subclass: #OpenCLMacOS32BitsInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLMacOS32BitsInterface class' category: 'OpenCL'!
OpenCLMacOS32BitsInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLUnix32BitsInterface category: 'OpenCL'!
OpenCLPrimitiveInterface subclass: #OpenCLUnix32BitsInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLUnix32BitsInterface class' category: 'OpenCL'!
OpenCLUnix32BitsInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLWindows32BitsInterface category: 'OpenCL'!
OpenCLPrimitiveInterface subclass: #OpenCLWindows32BitsInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLWindows32BitsInterface class' category: 'OpenCL'!
OpenCLWindows32BitsInterface class
	instanceVariableNames: ''!

!classDefinition: #OpenCLRefCounter category: 'OpenCL'!
Object subclass: #OpenCLRefCounter
	instanceVariableNames: 'refCount id'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL'!
!classDefinition: 'OpenCLRefCounter class' category: 'OpenCL'!
OpenCLRefCounter class
	instanceVariableNames: ''!

!classDefinition: #OpenCLExample category: 'OpenCL-Examples'!
Object subclass: #OpenCLExample
	instanceVariableNames: 'device context queue program'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLExample class' category: 'OpenCL-Examples'!
OpenCLExample class
	instanceVariableNames: ''!

!classDefinition: #AppleCopyBenchmark category: 'OpenCL-Examples'!
OpenCLExample subclass: #AppleCopyBenchmark
	instanceVariableNames: 'kernel inputBuffer outputBuffer lastEvent outputMatrix'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'AppleCopyBenchmark class' category: 'OpenCL-Examples'!
AppleCopyBenchmark class
	instanceVariableNames: ''!

!classDefinition: #DeformationGridExample category: 'OpenCL-Examples'!
OpenCLExample subclass: #DeformationGridExample
	instanceVariableNames: 'kernel input gridX gridY result resultExtent lastEvent outputImage'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'DeformationGridExample class' category: 'OpenCL-Examples'!
DeformationGridExample class
	instanceVariableNames: ''!

!classDefinition: #NormalizedCoordinatesExperiment category: 'OpenCL-Examples'!
OpenCLExample subclass: #NormalizedCoordinatesExperiment
	instanceVariableNames: 'kernel originalFrame processedFrame lastEvent'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'NormalizedCoordinatesExperiment class' category: 'OpenCL-Examples'!
NormalizedCoordinatesExperiment class
	instanceVariableNames: ''!

!classDefinition: #OpenCLBlur category: 'OpenCL-Examples'!
OpenCLExample subclass: #OpenCLBlur
	instanceVariableNames: 'horizontalKernel verticalKernel filterWeights filterWeightsBuffer imageExtent inputOutputImage tempImage lastEvent'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLBlur class' category: 'OpenCL-Examples'!
OpenCLBlur class
	instanceVariableNames: ''!

!classDefinition: #OpenCLBlur12 category: 'OpenCL-Examples'!
OpenCLExample subclass: #OpenCLBlur12
	instanceVariableNames: 'horizontalKernel verticalKernel filterWeights filterWeightsBuffer imageExtent inputOutputImage tempImage lastEvent'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLBlur12 class' category: 'OpenCL-Examples'!
OpenCLBlur12 class
	instanceVariableNames: ''!

!classDefinition: #OpenCLJuliaSet category: 'OpenCL-Examples'!
OpenCLExample subclass: #OpenCLJuliaSet
	instanceVariableNames: 'extent iterations outputForm workImage toneMapForm toneMapImage kernel lastEvent params paramsDirty paramsBuffer scale origin'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLJuliaSet class' category: 'OpenCL-Examples'!
OpenCLJuliaSet class
	instanceVariableNames: ''!

!classDefinition: #OpenCLFill256 category: 'OpenCL-Examples'!
Object subclass: #OpenCLFill256
	instanceVariableNames: 'device context queue program kernel1d kernel2d lastEvent buffer'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'OpenCL-Examples'!
!classDefinition: 'OpenCLFill256 class' category: 'OpenCL-Examples'!
OpenCLFill256 class
	instanceVariableNames: ''!

!classDefinition: #OpenCLPlugin category: 'OpenCL-Plugin'!
Object subclass: #OpenCLPlugin
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenCL-Plugin'!
!classDefinition: 'OpenCLPlugin class' category: 'OpenCL-Plugin'!
OpenCLPlugin class
	instanceVariableNames: ''!


!OpenCLConstants commentStamp: '<historical>' prior: 0!
Initialization is autogenerated. See also OpenCLPrimitiveInterface.

	OpenCLConstants clearConstantInitializers
	OpenCLConstants generateConstantInitializers!

!OpenCLError commentStamp: 'jcg 1/11/2010 23:22' prior: 0!
Error that knows how to look up a human-readable string corresponding to the OpenCL error code.!

!OpenCLJuliaSetMorph commentStamp: '<historical>' prior: 0!
OpenCLJuliaSetMorph new openInHand!

!CLBufferRegionStruct commentStamp: '<historical>' prior: 0!
Similar to CLImageFormatStruct and CLImageDescStruct. For OpenCL's cl_buffer_region

Declared in cl.h (unchanged in OpenCL 1.1, 1.2 and 2.0) as:

typedef struct _cl_buffer_region {
    size_t                  origin;
    size_t                  size;
} cl_buffer_region;!

!CLImageDescStruct commentStamp: '<historical>' prior: 0!
Similar to CLImageFormatStruct. For OpenCL's cl_image_desc

Declared in cl.h (OpenCL 1.2) as:

typedef struct _cl_image_desc {
    cl_mem_object_type      image_type;
    size_t                  image_width;
    size_t                  image_height;
    size_t                  image_depth;
    size_t                  image_array_size;
    size_t                  image_row_pitch;
    size_t                  image_slice_pitch;
    cl_uint                 num_mip_levels;
    cl_uint                 num_samples;
    cl_mem                  buffer;
} cl_image_desc;

Declared in cl.h (OpenCL 2.0) as:
typedef struct _cl_image_desc {
    cl_mem_object_type      image_type;
    size_t                  image_width;
    size_t                  image_height;
    size_t                  image_depth;
    size_t                  image_array_size;
    size_t                  image_row_pitch;
    size_t                  image_slice_pitch;
    cl_uint                 num_mip_levels;
    cl_uint                 num_samples;
#ifdef __GNUC__
    __extension__   /* Prevents warnings about anonymous union in -pedantic builds */
#endif
    union {
      cl_mem                  buffer;
      cl_mem                  mem_object;
    };
} cl_image_desc;!

!CLImageFormatStruct commentStamp: '<historical>' prior: 0!
For uniformity with the other OpenCL FFI types, this would be called CLImageFormat.  However, there is already an OpenCL constant with that name.  The exception is made less objectionable by the other FFI types being opaque handle types, whereas this is the only structure type with accessible fields.

Declared in cl.h (unchanged in OpenCL 1.0, 1.1, 1.2 and 2.0) as:

typedef struct _cl_image_format {
    cl_channel_order        image_channel_order;
    cl_channel_type         image_channel_data_type;
} cl_image_format;!

!OpenCLOpaqueHandle commentStamp: 'jcg 1/15/2010 01:18' prior: 0!
Private to the OpenCL framework; users of the framework will never directly touch one.

OpenCL uses several pointer-to-undefined-struct typedefs.  This is the base-class for corresponding FFI types.!

!CLCommandQueue commentStamp: '<historical>' prior: 0!
typedef struct _cl_command_queue *  cl_command_queue;!

!CLContext commentStamp: '<historical>' prior: 0!
typedef struct _cl_context *        cl_context;!

!CLDeviceID commentStamp: '<historical>' prior: 0!
typedef struct _cl_device_id *      cl_device_id;!

!CLEvent commentStamp: '<historical>' prior: 0!
typedef struct _cl_event *          cl_event;!

!CLKernel commentStamp: '<historical>' prior: 0!
typedef struct _cl_kernel *         cl_kernel;!

!CLMem commentStamp: '<historical>' prior: 0!
typedef struct _cl_mem *            cl_mem;!

!CLPlatformID commentStamp: '<historical>' prior: 0!
typedef struct _cl_platform_id *      cl_platform_id;!

!CLProgram commentStamp: '<historical>' prior: 0!
typedef struct _cl_program *        cl_program;!

!CLSampler commentStamp: '<historical>' prior: 0!
typedef struct _cl_sampler *        cl_sampler;!

!OpenCLObject commentStamp: 'jcg 1/13/2010 22:27' prior: 0!
Subclasses wrap an instance of the corresponding OpenCLOpaqueHandle type.  Provides convenience methods for calling FFI functions on the handle.!

!OpenCLDevice commentStamp: 'jcg 1/14/2010 01:58' prior: 0!
Encapsulates a CLDeviceID, and provides convenience methods for accessing its properties.  Perhaps these methods should simply be added to CLDeviceID, and this class deleted (except that we apparently can't add instance variables to subclasses of ExternalStructure, and still marshal them through the FFI)..!

!OpenCLPlatform commentStamp: 'jmv 9/16/2015 17:28' prior: 0!
Encapsulates a CLPlatformID, and provides convenience methods for accessing its properties.  Perhaps these methods should simply be added to CLPlatformID, and this class deleted  (except that we apparently can't add instance variables to subclasses of ExternalStructure, and still marshal them through the FFI).

OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].!

!OpenCLRefCountedObject commentStamp: 'jcg 1/15/2010 02:01' prior: 0!
Objects in the OpenCL Runtime layer are ref-counted (as opposed to platforms and devices in the Platform layer).  This class, along with OpenCLRefCounter, provides a framework for managing the ref-counts, as well as a finalization mechanism so that OpenCL resources are not leaked when these objects are garbage-collected while their ref-count is > 0.
!

!OpenCLContext commentStamp: 'jcg 1/14/2010 01:57' prior: 0!
Encapsulates a CLContext, and provides convenience methods for accessing its properties.  Perhaps these methods should simply be added to CLContext, and this class deleted  (except that we apparently can't add instance variables to subclasses of ExternalStructure, and still marshal them through the FFI).!

!OpenCLContextObject commentStamp: 'jcg 2/12/2010 09:20' prior: 0!
Represents an object that is associated with a particular OpenCL context.  When destroyed, the reference to the context is released.!

!OpenCLBuffer commentStamp: '<historical>' prior: 0!
Encapsulates a CLMem that has been allocated as a buffer (as opposed to an image), and provides convenience methods for accessing its properties.  See the "buffer creation" methods in OpenCLContext.  

NOTE: we don't expose all of OpenCL's functionality for instantiating new buffers, in particular buffers allocated in host memory. But we do allow wrapping a Smalltalk object. For this, we pin the memory and unpin it on buffer destruction.!

!OpenCLCommandQueue commentStamp: 'jcg 1/14/2010 01:57' prior: 0!
Encapsulates a CLCommandQueue and provides convenience methods for accessing its properties.!

!OpenCLEvent commentStamp: 'jcg 1/14/2010 01:57' prior: 0!
Encapsulates a CLEvent and provides convenience methods for accessing its properties.!

!OpenCLMapEvent commentStamp: 'jcg 1/30/2010 17:30' prior: 0!
The commands to map images and buffers return a pointer.  However, this pointer is not valid until the mapping is complete.  Therefore, we encapsulate the pointer within a special event-type; the pointer is not made available until the event's enqueued command has completed.!

!OpenCLPrimitiveInterface commentStamp: '<historical>' prior: 0!
Subclasses hold mainly autogenerated methods. See also OpenCLConstants.

	OpenCLPrimitiveInterface allSubclassesDo: [ :cls | cls clearFFICalls ].
	OpenCLPrimitiveInterface allSubclassesDo: [ :cls | cls supportedPlatform ifNotNil: [ cls generateFFICalls ]].

	OpenCLConstants clearConstantInitializers
	OpenCLConstants generateConstantInitializers!

!OpenCL64BitsPrimitiveInterface commentStamp: '<historical>' prior: 0!
64 bit platforms!

!OpenCLWindowsInterface commentStamp: 'jmv 1/13/2017 13:20:39' prior: 0!
A note for OpenCL on Windows.

It seems that Windows expects any GPU operation to finish in some time interval. They call this "Timeout Detection and Recovery". The timeout for "busy gpu" after which the GPU is reset is configurabe via some registry keys. See https://msdn.microsoft.com/en-us/library/windows/hardware/ff570087(v=vs.85).aspx .

There are two approaches to deal with this. One is to break GPU processing in chunks small enough for them to finish within the default timeout. Another alternative is to ask the user to tweak the appropriate entries in the Windows registry.

This issue and solutions were found by Alejandra De Bonis at CaesarSystems. Thanks!!!

!OpenCLWindows32BitsInterface commentStamp: '<historical>' prior: 0!
A note for OpenCL on Windows.

It seems that Windows expects any GPU operation to finish in some time interval. They call this "Timeout Detection and Recovery". The timeout for "busy gpu" after which the GPU is reset is configurabe via some registry keys. See https://msdn.microsoft.com/en-us/library/windows/hardware/ff570087(v=vs.85).aspx .

There are two approaches to deal with this. One is to break GPU processing in chunks small enough for them to finish within the default timeout. Another alternative is to ask the user to tweak the appropriate entries in the Windows registry.

This issue and solutions were found by Alejandra De Bonis at CaesarSystems. Thanks!!!

!OpenCLRefCounter commentStamp: 'jcg 1/15/2010 01:18' prior: 0!
Private to the OpenCL framework; users of the framework will never directly touch one.

Keeps track of the reference-count of an OpenCLRefCountedObject.  The reason that this is a separate class is that it also acts as the executor for finalizing the object.
!

!OpenCLExample commentStamp: 'jmv 10/29/2015 11:57' prior: 0!
This class includes ivars that any (all?) OpenCL examples should have.
Limitations:
- Handle just one device (it is possible to have a single program use several devices)

	device 	-> 		GPU or CPU
	context 	-> 		Instance of OpenCLContext. Built for our device.
	queue 	-> 		Instance of OpenCLCommandQueue. There is a queue for each device. Just one here.
	program 	-> 		Instance of OpenCLProgram. Holds source code, creates kernels.!

!AppleCopyBenchmark commentStamp: 'jmv 9/9/2015 14:50' prior: 0!
"
---EXPERIMENTAL!!---

As in https://developer.apple.com/library/mac/documentation/Performance/Conceptual/OpenCL_MacProgGuide/TuningPerformanceOntheGPU/TuningPerformanceOntheGPU.html#//apple_ref/doc/uid/TP40008312-CH22-SW11 "

| lena bench result |
lena _ FloatImage lena scaledBy: 4.005.
bench _ AppleCopyBenchmark new.
	bench input: lena.
AndreasSystemProfiler spyOn: [
	100 timesRepeat: [
		bench run ]
].
	result _ bench result.
result display!

!DeformationGridExample commentStamp: '<historical>' prior: 0!
---EXPERIMENTAL!!---

Parece que no tenemos OpenCL 1.2. Bueno usemos la api de 1.0 o 1.1... (O sea clCreateImage2D() y no clCreateImage() )

En este punto, esto fue abandonado. Eliminarlo, o completarlo... Ahora si tenemos OpenCL 1.2 y 2.0.!

!NormalizedCoordinatesExperiment commentStamp: '<historical>' prior: 0!
Try evaluating:
	NormalizedCoordinatesExperiment go!

!OpenCLBlur commentStamp: '<historical>' prior: 0!
Pretty much the simplest thing you can do with OpenCL: a separable Gaussian blur.

Try evaluating:
	OpenCLBlur blurDisplay!

!OpenCLBlur12 commentStamp: '<historical>' prior: 0!
Pretty much the simplest thing you can do with OpenCL: a separable Gaussian blur.
Adapted to OpenCL 1.2. The only change is in #setupImages:, use clCreateImage() and not the (now deprecated) clCreateImage2D()

Try evaluating:
	OpenCLBlur12 blurDisplay!

!OpenCLJuliaSet commentStamp: '<historical>' prior: 0!
OpenCLJuliaSet example1!

!OpenCLFill256 commentStamp: 'jmv 8/5/2015 16:49' prior: 0!
A very stupid example that fills a 256-element array with the values 0... 255.  This was done as a sanity check (I later discovered that I was sane, and that the Nvidia OpenCL can't yet have a kernel write to an image).

OpenCLFill256 new run1d
OpenCLFill256 new run2d!

!OpenCLPlugin commentStamp: 'jcg 1/21/2010 02:48' prior: 0!
Placeholder for a subclass of InterpreterPlugin.

Responsibilities:
	- implement surfaces a-la SurfacePlugin so that we can make a SqueakForm that refers directly to a mapped OpenCL 2D image
	- primitive support to allow OpenCLEvent class>>waitForEvents: to block a single Squeak Process, rather than the main VM thread.  To do this, we need a separate native thread (maybe from a thread-pool?) that will signal a Squeak semaphore... the Process waiting for the primitive will block waiting for that semaphore to be signalled.  
	
	for blocking-waits on a command-queue that occur in a different native thread.  Probably make a subclass of command-queue whose instances register themselves with the plugin, and!

!CLImageDescStruct methodsFor: 'printing' stamp: 'jmv 10/30/2015 12:53'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: (self class image_type_String: self image_type);
		nextPutAll: ' width:'; print: self image_width;
		nextPutAll: ' height:'; print: self image_height;
		nextPutAll: ' depth'; print: self image_depth;
		nextPutAll: ' arraySize:'; print: self image_array_size;
		nextPutAll: ' rowPitch:'; print: self image_row_pitch;
		nextPutAll: ' slicePitch:'; print: self image_slice_pitch;
		nextPutAll: ' mipLevels:'; print: self num_mip_levels;
		nextPutAll: ' numSamples:'; print: self num_samples;
		nextPutAll: ' buffer:'; print: self buffer;
		nextPut: $)! !

!CLImageFormatStruct methodsFor: 'printing' stamp: 'jmv 10/29/2015 15:11'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: (self class image_channel_order_String: self image_channel_order).
	aStream space.
	aStream nextPutAll: (self class image_channel_data_type_String: self image_channel_data_type).
	aStream nextPut: $).! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
name
	"Device name string."
	^self stringProperty: CL_DEVICE_NAME! !

!OpenCLDevice methodsFor: 'printing' stamp: 'pb 6/18/2019 02:00:37'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(; print: self name; nextPut:$)! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jcg 1/10/2010 02:22'!
name
	^name! !

!OpenCLImage2D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
format
	^format! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
format
	^format! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 7/1/2019 17:54:48'!
shutDown: quitting
	"We assume that the image is being saved.  When it is restarted, we want the finalization-registry to be empty.  Otherwise, there is a race-condition where the finalization-process might run before the startup-list is processed, and we might crash because we try to free OpenCL objects whose handles are no longer valid.  Also see #startUp:."
	self finalizationRegistry removeAll.
	OpenCLPlatform preferredDevice: nil! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 20:16'!
startUp: resuming
	"Behave differently depending on whether we are restarting a saved image, or are continuing without quitting."
	resuming 
		ifTrue:[
			"Each time we restart the image, we flush the installed library so that 
			 we are sure to use the proper library for the current platform."
			ActiveSubclass := nil.
			
			"Any existing OpenCLObjects are invalid; mark them as such."
			OpenCLRefCounter allInstancesDo:[:counter| counter invalidateOnStartUp].
			OpenCLObject allSubInstancesDo: [:obj | obj invalidateOnStartUp].
		]
		ifFalse: [
			"See #shutDown:... there we cleaned out our finalization registry to avoid a 
			 crash-causing race-condition when restarting a saved image.  Now we put 
			 all of the objects back, to ensure that they're finalized."
			OpenCLRefCountedObject allSubInstancesDo: [:obj | obj registerForFinalization]
		].! !

!OpenCLRefCounter methodsFor: 'printing' stamp: 'jcg 1/15/2010 00:57'!
printOn: aStream
	aStream nextPutAll: '[ref-count: '.
	refCount printOn: aStream.
	aStream nextPut: $]
! !

!OpenCLConstants class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:19:32'!
clearConstantInitializers
	"
	OpenCLConstants clearConstantInitializers
	"
	| cls |
	cls _ OpenCLConstants class.
	cls selectors copy do: [ :sel |
		((cls whichCategoryIncludesSelector: sel) = 'class initialization')
			ifTrue: [cls removeSelector: sel ]].
	cls organization removeEmptyCategories! !

!OpenCLConstants class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:21:00'!
generateConstantInitializers
"
	OpenCLConstants generateConstantInitializers
"
	"The input should be a String containing the portion of cl.h with the constant definitions (no typedefs, function declarations, etc.)"
	| declarations existingClassVars generatedMethods methodString  |
	self readConstantsFrom: OpenCLPrimitiveInterface constantDeclarationsInC into: [ :categories :sortedCategories |

		"We'll only add class variables that don't already exist."
		existingClassVars := OpenCLConstants classVarNames.

		"Generate the source code for each class-var initialization method."
		generatedMethods := OrderedCollection new.
		sortedCategories do: [ :categoryName |
			declarations := categories at: categoryName.
			methodString := String streamContents: [:stream |
				"Write the method name and comment.."
				stream nextPutAll: 'initializeClassVariables'.
				(generatedMethods size + 1) printOn: stream base: 10 length: 2 padded: true.
				stream
					newLine; tab; 
					nextPutAll: 
						'"Auto-generated initialization of OpenCL constants for ', 
						categoryName surroundedBySingleQuotes, '"';
					newLine.
				"Write initializer for each declaration.  Create vars if necessary."
				declarations do: [:decl "an Association" |
					(existingClassVars includes: decl key) ifFalse: [
						OpenCLConstants addClassVarName: decl key
					].
					stream newLine; tab; nextPutAll: decl key, ' _ ', decl value, '.'.
				].
				stream newLine.
			].
			generatedMethods add: methodString.
		].
	].

	"Generate the #initialize method that will call the other generate methods."
	methodString := String streamContents: [:stream |
		stream 
			nextPutAll: 'initialize';
			newLine; tab; nextPutAll: '"Call auto-generated class-var initializers."'.
		1 to: generatedMethods size do: [:i |
			stream newLine; tab; nextPutAll: 'self initializeClassVariables'.
			i printOn: stream base: 10 length: 2 padded: true.
			stream nextPutAll: '.'.
		]
	].
	generatedMethods add: methodString.
	
	"Compile all methods and and run the initializer."
	generatedMethods do: [:src |
		OpenCLConstants class 
			compile: src 
			classified: #'class initialization'
			withStamp: nil
			notifying: nil
			logSource: true.
	].
	OpenCLConstants initialize.
	^generatedMethods! !

!OpenCLConstants class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:21:18'!
readConstantsFrom: aString into: aBlock
	"The input should be a String containing the portion of cl.h with the constant definitions (no typedefs, function declarations, etc.)"
	| category allCategories sortedCategories  |
	category := 'unknown category'.
	allCategories := Dictionary new.
	sortedCategories := OrderedCollection new.
	aString linesDo: [:line |
		(line beginsWith: '//') ifTrue: [
			category := (line copyFrom: 3 to: line size) withBlanksTrimmed.
		].
		(line beginsWith: '#define') ifTrue: [
			| tokens constName constValue categoryDeclarations |
			tokens := line findTokens: Character separators, '()'.
			"Both the name and value will need to be processed further."
			constName := tokens second.
			constValue := tokens copyFrom: 3 to: tokens size.
			constName := constName asSymbol.
			"Create initializer code for variables.  Some are decimal, some hex-literals, 
			 and some are bit-shifts; we need to deal with all three."
			constValue size = 1
				ifTrue: ["decimal or hex literal"
					constValue := constValue first copyReplaceAll: '0x' with: '16r'.
				]
				ifFalse: ["bit-shift"
					constValue := '1 bitShift: ', constValue last
				].
			"Get the collection of method-source for this category (create one
			 if necessary), the add to it an association between the name and value."
			categoryDeclarations := allCategories 
										at: category 
										ifAbsentPut: [
											sortedCategories add: category.
											OrderedCollection new].
			categoryDeclarations add: constName -> constValue.
		]
	].
	aBlock value: allCategories value: sortedCategories! !

!OpenCLConstants class methodsFor: 'class initialization'!
initialize
	"Call auto-generated class-var initializers."
	self initializeClassVariables01.
	self initializeClassVariables02.
	self initializeClassVariables03.
	self initializeClassVariables04.
	self initializeClassVariables05.
	self initializeClassVariables06.
	self initializeClassVariables07.
	self initializeClassVariables08.
	self initializeClassVariables09.
	self initializeClassVariables10.
	self initializeClassVariables11.
	self initializeClassVariables12.
	self initializeClassVariables13.
	self initializeClassVariables14.
	self initializeClassVariables15.
	self initializeClassVariables16.
	self initializeClassVariables17.
	self initializeClassVariables18.
	self initializeClassVariables19.
	self initializeClassVariables20.
	self initializeClassVariables21.
	self initializeClassVariables22.
	self initializeClassVariables23.
	self initializeClassVariables24.
	self initializeClassVariables25.
	self initializeClassVariables26.
	self initializeClassVariables27.
	self initializeClassVariables28.
	self initializeClassVariables29.
	self initializeClassVariables30.
	self initializeClassVariables31.
	self initializeClassVariables32.
	self initializeClassVariables33.
	self initializeClassVariables34.
	self initializeClassVariables35.
	self initializeClassVariables36.
	self initializeClassVariables37.
	self initializeClassVariables38.
	self initializeClassVariables39.
	self initializeClassVariables40.
	self initializeClassVariables41.
	self initializeClassVariables42.
	self initializeClassVariables43.
	self initializeClassVariables44.
	self initializeClassVariables45.
	self initializeClassVariables46.
	self initializeClassVariables47.
	self initializeClassVariables48.
	self initializeClassVariables49.
	self initializeClassVariables50.! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables01
	"Auto-generated initialization of OpenCL constants for 'Error Codes'"

	CL_SUCCESS _ 0.
	CL_DEVICE_NOT_FOUND _ -1.
	CL_DEVICE_NOT_AVAILABLE _ -2.
	CL_COMPILER_NOT_AVAILABLE _ -3.
	CL_MEM_OBJECT_ALLOCATION_FAILURE _ -4.
	CL_OUT_OF_RESOURCES _ -5.
	CL_OUT_OF_HOST_MEMORY _ -6.
	CL_PROFILING_INFO_NOT_AVAILABLE _ -7.
	CL_MEM_COPY_OVERLAP _ -8.
	CL_IMAGE_FORMAT_MISMATCH _ -9.
	CL_IMAGE_FORMAT_NOT_SUPPORTED _ -10.
	CL_BUILD_PROGRAM_FAILURE _ -11.
	CL_MAP_FAILURE _ -12.
	CL_MISALIGNED_SUB_BUFFER_OFFSET _ -13.
	CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST _ -14.
	CL_COMPILE_PROGRAM_FAILURE _ -15.
	CL_LINKER_NOT_AVAILABLE _ -16.
	CL_LINK_PROGRAM_FAILURE _ -17.
	CL_DEVICE_PARTITION_FAILED _ -18.
	CL_KERNEL_ARG_INFO_NOT_AVAILABLE _ -19.
	CL_INVALID_VALUE _ -30.
	CL_INVALID_DEVICE_TYPE _ -31.
	CL_INVALID_PLATFORM _ -32.
	CL_INVALID_DEVICE _ -33.
	CL_INVALID_CONTEXT _ -34.
	CL_INVALID_QUEUE_PROPERTIES _ -35.
	CL_INVALID_COMMAND_QUEUE _ -36.
	CL_INVALID_HOST_PTR _ -37.
	CL_INVALID_MEM_OBJECT _ -38.
	CL_INVALID_IMAGE_FORMAT_DESCRIPTOR _ -39.
	CL_INVALID_IMAGE_SIZE _ -40.
	CL_INVALID_SAMPLER _ -41.
	CL_INVALID_BINARY _ -42.
	CL_INVALID_BUILD_OPTIONS _ -43.
	CL_INVALID_PROGRAM _ -44.
	CL_INVALID_PROGRAM_EXECUTABLE _ -45.
	CL_INVALID_KERNEL_NAME _ -46.
	CL_INVALID_KERNEL_DEFINITION _ -47.
	CL_INVALID_KERNEL _ -48.
	CL_INVALID_ARG_INDEX _ -49.
	CL_INVALID_ARG_VALUE _ -50.
	CL_INVALID_ARG_SIZE _ -51.
	CL_INVALID_KERNEL_ARGS _ -52.
	CL_INVALID_WORK_DIMENSION _ -53.
	CL_INVALID_WORK_GROUP_SIZE _ -54.
	CL_INVALID_WORK_ITEM_SIZE _ -55.
	CL_INVALID_GLOBAL_OFFSET _ -56.
	CL_INVALID_EVENT_WAIT_LIST _ -57.
	CL_INVALID_EVENT _ -58.
	CL_INVALID_OPERATION _ -59.
	CL_INVALID_GL_OBJECT _ -60.
	CL_INVALID_BUFFER_SIZE _ -61.
	CL_INVALID_MIP_LEVEL _ -62.
	CL_INVALID_GLOBAL_WORK_SIZE _ -63.
	CL_INVALID_PROPERTY _ -64.
	CL_INVALID_IMAGE_DESCRIPTOR _ -65.
	CL_INVALID_COMPILER_OPTIONS _ -66.
	CL_INVALID_LINKER_OPTIONS _ -67.
	CL_INVALID_DEVICE_PARTITION_COUNT _ -68.
	CL_INVALID_PIPE_SIZE _ -69.
	CL_INVALID_DEVICE_QUEUE _ -70.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables02
	"Auto-generated initialization of OpenCL constants for 'OpenCL Version'"

	CL_VERSION_1_0 _ 1.
	CL_VERSION_1_1 _ 1.
	CL_VERSION_1_2 _ 1.
	CL_VERSION_2_0 _ 1.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables03
	"Auto-generated initialization of OpenCL constants for 'cl_bool'"

	CL_FALSE _ 0.
	CL_TRUE _ 1.
	CL_BLOCKING _ CL_TRUE.
	CL_NON_BLOCKING _ CL_FALSE.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables04
	"Auto-generated initialization of OpenCL constants for 'cl_platform_info'"

	CL_PLATFORM_PROFILE _ 16r0900.
	CL_PLATFORM_VERSION _ 16r0901.
	CL_PLATFORM_NAME _ 16r0902.
	CL_PLATFORM_VENDOR _ 16r0903.
	CL_PLATFORM_EXTENSIONS _ 16r0904.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables05
	"Auto-generated initialization of OpenCL constants for 'cl_device_type - bitfield'"

	CL_DEVICE_TYPE_DEFAULT _ 1 bitShift: 0.
	CL_DEVICE_TYPE_CPU _ 1 bitShift: 1.
	CL_DEVICE_TYPE_GPU _ 1 bitShift: 2.
	CL_DEVICE_TYPE_ACCELERATOR _ 1 bitShift: 3.
	CL_DEVICE_TYPE_CUSTOM _ 1 bitShift: 4.
	CL_DEVICE_TYPE_ALL _ 16rFFFFFFFF.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables06
	"Auto-generated initialization of OpenCL constants for 'cl_device_info'"

	CL_DEVICE_TYPE _ 16r1000.
	CL_DEVICE_VENDOR_ID _ 16r1001.
	CL_DEVICE_MAX_COMPUTE_UNITS _ 16r1002.
	CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS _ 16r1003.
	CL_DEVICE_MAX_WORK_GROUP_SIZE _ 16r1004.
	CL_DEVICE_MAX_WORK_ITEM_SIZES _ 16r1005.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR _ 16r1006.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT _ 16r1007.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT _ 16r1008.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG _ 16r1009.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT _ 16r100A.
	CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE _ 16r100B.
	CL_DEVICE_MAX_CLOCK_FREQUENCY _ 16r100C.
	CL_DEVICE_ADDRESS_BITS _ 16r100D.
	CL_DEVICE_MAX_READ_IMAGE_ARGS _ 16r100E.
	CL_DEVICE_MAX_WRITE_IMAGE_ARGS _ 16r100F.
	CL_DEVICE_MAX_MEM_ALLOC_SIZE _ 16r1010.
	CL_DEVICE_IMAGE2D_MAX_WIDTH _ 16r1011.
	CL_DEVICE_IMAGE2D_MAX_HEIGHT _ 16r1012.
	CL_DEVICE_IMAGE3D_MAX_WIDTH _ 16r1013.
	CL_DEVICE_IMAGE3D_MAX_HEIGHT _ 16r1014.
	CL_DEVICE_IMAGE3D_MAX_DEPTH _ 16r1015.
	CL_DEVICE_IMAGE_SUPPORT _ 16r1016.
	CL_DEVICE_MAX_PARAMETER_SIZE _ 16r1017.
	CL_DEVICE_MAX_SAMPLERS _ 16r1018.
	CL_DEVICE_MEM_BASE_ADDR_ALIGN _ 16r1019.
	CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE _ 16r101A.
	CL_DEVICE_SINGLE_FP_CONFIG _ 16r101B.
	CL_DEVICE_GLOBAL_MEM_CACHE_TYPE _ 16r101C.
	CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE _ 16r101D.
	CL_DEVICE_GLOBAL_MEM_CACHE_SIZE _ 16r101E.
	CL_DEVICE_GLOBAL_MEM_SIZE _ 16r101F.
	CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE _ 16r1020.
	CL_DEVICE_MAX_CONSTANT_ARGS _ 16r1021.
	CL_DEVICE_LOCAL_MEM_TYPE _ 16r1022.
	CL_DEVICE_LOCAL_MEM_SIZE _ 16r1023.
	CL_DEVICE_ERROR_CORRECTION_SUPPORT _ 16r1024.
	CL_DEVICE_PROFILING_TIMER_RESOLUTION _ 16r1025.
	CL_DEVICE_ENDIAN_LITTLE _ 16r1026.
	CL_DEVICE_AVAILABLE _ 16r1027.
	CL_DEVICE_COMPILER_AVAILABLE _ 16r1028.
	CL_DEVICE_EXECUTION_CAPABILITIES _ 16r1029.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables07
	"Auto-generated initialization of OpenCL constants for 'CL_DEVICE_QUEUE_PROPERTIES is deprecated!!'"

	CL_DEVICE_QUEUE_PROPERTIES _ 16r102A.
	CL_DEVICE_QUEUE_ON_HOST_PROPERTIES _ 16r102A.
	CL_DEVICE_NAME _ 16r102B.
	CL_DEVICE_VENDOR _ 16r102C.
	CL_DRIVER_VERSION _ 16r102D.
	CL_DEVICE_PROFILE _ 16r102E.
	CL_DEVICE_VERSION _ 16r102F.
	CL_DEVICE_EXTENSIONS _ 16r1030.
	CL_DEVICE_PLATFORM _ 16r1031.
	CL_DEVICE_DOUBLE_FP_CONFIG _ 16r1032.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables08
	"Auto-generated initialization of OpenCL constants for '0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG'"

	CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF _ 16r1034.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables09
	"Auto-generated initialization of OpenCL constants for 'CL_DEVICE_HOST_UNIFIED_MEMORY is deprecated!!'"

	CL_DEVICE_HOST_UNIFIED_MEMORY _ 16r1035.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR _ 16r1036.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT _ 16r1037.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_INT _ 16r1038.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG _ 16r1039.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT _ 16r103A.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE _ 16r103B.
	CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF _ 16r103C.
	CL_DEVICE_OPENCL_C_VERSION _ 16r103D.
	CL_DEVICE_LINKER_AVAILABLE _ 16r103E.
	CL_DEVICE_BUILT_IN_KERNELS _ 16r103F.
	CL_DEVICE_IMAGE_MAX_BUFFER_SIZE _ 16r1040.
	CL_DEVICE_IMAGE_MAX_ARRAY_SIZE _ 16r1041.
	CL_DEVICE_PARENT_DEVICE _ 16r1042.
	CL_DEVICE_PARTITION_MAX_SUB_DEVICES _ 16r1043.
	CL_DEVICE_PARTITION_PROPERTIES _ 16r1044.
	CL_DEVICE_PARTITION_AFFINITY_DOMAIN _ 16r1045.
	CL_DEVICE_PARTITION_TYPE _ 16r1046.
	CL_DEVICE_REFERENCE_COUNT _ 16r1047.
	CL_DEVICE_PREFERRED_INTEROP_USER_SYNC _ 16r1048.
	CL_DEVICE_PRINTF_BUFFER_SIZE _ 16r1049.
	CL_DEVICE_IMAGE_PITCH_ALIGNMENT _ 16r104A.
	CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT _ 16r104B.
	CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS _ 16r104C.
	CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE _ 16r104D.
	CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES _ 16r104E.
	CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE _ 16r104F.
	CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE _ 16r1050.
	CL_DEVICE_MAX_ON_DEVICE_QUEUES _ 16r1051.
	CL_DEVICE_MAX_ON_DEVICE_EVENTS _ 16r1052.
	CL_DEVICE_SVM_CAPABILITIES _ 16r1053.
	CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE _ 16r1054.
	CL_DEVICE_MAX_PIPE_ARGS _ 16r1055.
	CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS _ 16r1056.
	CL_DEVICE_PIPE_MAX_PACKET_SIZE _ 16r1057.
	CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT _ 16r1058.
	CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT _ 16r1059.
	CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT _ 16r105A.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables10
	"Auto-generated initialization of OpenCL constants for 'cl_device_fp_config - bitfield'"

	CL_FP_DENORM _ 1 bitShift: 0.
	CL_FP_INF_NAN _ 1 bitShift: 1.
	CL_FP_ROUND_TO_NEAREST _ 1 bitShift: 2.
	CL_FP_ROUND_TO_ZERO _ 1 bitShift: 3.
	CL_FP_ROUND_TO_INF _ 1 bitShift: 4.
	CL_FP_FMA _ 1 bitShift: 5.
	CL_FP_SOFT_FLOAT _ 1 bitShift: 6.
	CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT _ 1 bitShift: 7.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables11
	"Auto-generated initialization of OpenCL constants for 'cl_device_mem_cache_type'"

	CL_NONE _ 16r0.
	CL_READ_ONLY_CACHE _ 16r1.
	CL_READ_WRITE_CACHE _ 16r2.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables12
	"Auto-generated initialization of OpenCL constants for 'cl_device_local_mem_type'"

	CL_LOCAL _ 16r1.
	CL_GLOBAL _ 16r2.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables13
	"Auto-generated initialization of OpenCL constants for 'cl_device_exec_capabilities - bitfield'"

	CL_EXEC_KERNEL _ 1 bitShift: 0.
	CL_EXEC_NATIVE_KERNEL _ 1 bitShift: 1.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables14
	"Auto-generated initialization of OpenCL constants for 'cl_command_queue_properties - bitfield'"

	CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE _ 1 bitShift: 0.
	CL_QUEUE_PROFILING_ENABLE _ 1 bitShift: 1.
	CL_QUEUE_ON_DEVICE _ 1 bitShift: 2.
	CL_QUEUE_ON_DEVICE_DEFAULT _ 1 bitShift: 3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables15
	"Auto-generated initialization of OpenCL constants for 'cl_context_info'"

	CL_CONTEXT_REFERENCE_COUNT _ 16r1080.
	CL_CONTEXT_DEVICES _ 16r1081.
	CL_CONTEXT_PROPERTIES _ 16r1082.
	CL_CONTEXT_NUM_DEVICES _ 16r1083.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables16
	"Auto-generated initialization of OpenCL constants for 'cl_context_properties'"

	CL_CONTEXT_PLATFORM _ 16r1084.
	CL_CONTEXT_INTEROP_USER_SYNC _ 16r1085.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables17
	"Auto-generated initialization of OpenCL constants for 'cl_device_partition_property'"

	CL_DEVICE_PARTITION_EQUALLY _ 16r1086.
	CL_DEVICE_PARTITION_BY_COUNTS _ 16r1087.
	CL_DEVICE_PARTITION_BY_COUNTS_LIST_END _ 16r0.
	CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN _ 16r1088.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables18
	"Auto-generated initialization of OpenCL constants for 'cl_device_affinity_domain'"

	CL_DEVICE_AFFINITY_DOMAIN_NUMA _ 1 bitShift: 0.
	CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE _ 1 bitShift: 1.
	CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE _ 1 bitShift: 2.
	CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE _ 1 bitShift: 3.
	CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE _ 1 bitShift: 4.
	CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE _ 1 bitShift: 5.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables19
	"Auto-generated initialization of OpenCL constants for 'cl_device_svm_capabilities'"

	CL_DEVICE_SVM_COARSE_GRAIN_BUFFER _ 1 bitShift: 0.
	CL_DEVICE_SVM_FINE_GRAIN_BUFFER _ 1 bitShift: 1.
	CL_DEVICE_SVM_FINE_GRAIN_SYSTEM _ 1 bitShift: 2.
	CL_DEVICE_SVM_ATOMICS _ 1 bitShift: 3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables20
	"Auto-generated initialization of OpenCL constants for 'cl_command_queue_info'"

	CL_QUEUE_CONTEXT _ 16r1090.
	CL_QUEUE_DEVICE _ 16r1091.
	CL_QUEUE_REFERENCE_COUNT _ 16r1092.
	CL_QUEUE_PROPERTIES _ 16r1093.
	CL_QUEUE_SIZE _ 16r1094.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables21
	"Auto-generated initialization of OpenCL constants for 'cl_mem_flags and cl_svm_mem_flags - bitfield'"

	CL_MEM_READ_WRITE _ 1 bitShift: 0.
	CL_MEM_WRITE_ONLY _ 1 bitShift: 1.
	CL_MEM_READ_ONLY _ 1 bitShift: 2.
	CL_MEM_USE_HOST_PTR _ 1 bitShift: 3.
	CL_MEM_ALLOC_HOST_PTR _ 1 bitShift: 4.
	CL_MEM_COPY_HOST_PTR _ 1 bitShift: 5.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables22
	"Auto-generated initialization of OpenCL constants for 'reserved                                         (1 << 6)'"

	CL_MEM_HOST_WRITE_ONLY _ 1 bitShift: 7.
	CL_MEM_HOST_READ_ONLY _ 1 bitShift: 8.
	CL_MEM_HOST_NO_ACCESS _ 1 bitShift: 9.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables23
	"Auto-generated initialization of OpenCL constants for 'used by cl_svm_mem_flags only'"

	CL_MEM_SVM_FINE_GRAIN_BUFFER _ 1 bitShift: 10.
	CL_MEM_SVM_ATOMICS _ 1 bitShift: 11.
	CL_MEM_KERNEL_READ_AND_WRITE _ 1 bitShift: 12.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables24
	"Auto-generated initialization of OpenCL constants for 'cl_mem_migration_flags - bitfield'"

	CL_MIGRATE_MEM_OBJECT_HOST _ 1 bitShift: 0.
	CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED _ 1 bitShift: 1.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables25
	"Auto-generated initialization of OpenCL constants for 'cl_channel_order'"

	CL_R _ 16r10B0.
	CL_A _ 16r10B1.
	CL_RG _ 16r10B2.
	CL_RA _ 16r10B3.
	CL_RGB _ 16r10B4.
	CL_RGBA _ 16r10B5.
	CL_BGRA _ 16r10B6.
	CL_ARGB _ 16r10B7.
	CL_INTENSITY _ 16r10B8.
	CL_LUMINANCE _ 16r10B9.
	CL_Rx _ 16r10BA.
	CL_RGx _ 16r10BB.
	CL_RGBx _ 16r10BC.
	CL_DEPTH _ 16r10BD.
	CL_DEPTH_STENCIL _ 16r10BE.
	CL_sRGB _ 16r10BF.
	CL_sRGBx _ 16r10C0.
	CL_sRGBA _ 16r10C1.
	CL_sBGRA _ 16r10C2.
	CL_ABGR _ 16r10C3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables26
	"Auto-generated initialization of OpenCL constants for 'cl_channel_type'"

	CL_SNORM_INT8 _ 16r10D0.
	CL_SNORM_INT16 _ 16r10D1.
	CL_UNORM_INT8 _ 16r10D2.
	CL_UNORM_INT16 _ 16r10D3.
	CL_UNORM_SHORT_565 _ 16r10D4.
	CL_UNORM_SHORT_555 _ 16r10D5.
	CL_UNORM_INT_101010 _ 16r10D6.
	CL_SIGNED_INT8 _ 16r10D7.
	CL_SIGNED_INT16 _ 16r10D8.
	CL_SIGNED_INT32 _ 16r10D9.
	CL_UNSIGNED_INT8 _ 16r10DA.
	CL_UNSIGNED_INT16 _ 16r10DB.
	CL_UNSIGNED_INT32 _ 16r10DC.
	CL_HALF_FLOAT _ 16r10DD.
	CL_FLOAT _ 16r10DE.
	CL_UNORM_INT24 _ 16r10DF.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables27
	"Auto-generated initialization of OpenCL constants for 'cl_mem_object_type'"

	CL_MEM_OBJECT_BUFFER _ 16r10F0.
	CL_MEM_OBJECT_IMAGE2D _ 16r10F1.
	CL_MEM_OBJECT_IMAGE3D _ 16r10F2.
	CL_MEM_OBJECT_IMAGE2D_ARRAY _ 16r10F3.
	CL_MEM_OBJECT_IMAGE1D _ 16r10F4.
	CL_MEM_OBJECT_IMAGE1D_ARRAY _ 16r10F5.
	CL_MEM_OBJECT_IMAGE1D_BUFFER _ 16r10F6.
	CL_MEM_OBJECT_PIPE _ 16r10F7.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables28
	"Auto-generated initialization of OpenCL constants for 'cl_mem_info'"

	CL_MEM_TYPE _ 16r1100.
	CL_MEM_FLAGS _ 16r1101.
	CL_MEM_SIZE _ 16r1102.
	CL_MEM_HOST_PTR _ 16r1103.
	CL_MEM_MAP_COUNT _ 16r1104.
	CL_MEM_REFERENCE_COUNT _ 16r1105.
	CL_MEM_CONTEXT _ 16r1106.
	CL_MEM_ASSOCIATED_MEMOBJECT _ 16r1107.
	CL_MEM_OFFSET _ 16r1108.
	CL_MEM_USES_SVM_POINTER _ 16r1109.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables29
	"Auto-generated initialization of OpenCL constants for 'cl_image_info'"

	CL_IMAGE_FORMAT _ 16r1110.
	CL_IMAGE_ELEMENT_SIZE _ 16r1111.
	CL_IMAGE_ROW_PITCH _ 16r1112.
	CL_IMAGE_SLICE_PITCH _ 16r1113.
	CL_IMAGE_WIDTH _ 16r1114.
	CL_IMAGE_HEIGHT _ 16r1115.
	CL_IMAGE_DEPTH _ 16r1116.
	CL_IMAGE_ARRAY_SIZE _ 16r1117.
	CL_IMAGE_BUFFER _ 16r1118.
	CL_IMAGE_NUM_MIP_LEVELS _ 16r1119.
	CL_IMAGE_NUM_SAMPLES _ 16r111A.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables30
	"Auto-generated initialization of OpenCL constants for 'cl_pipe_info'"

	CL_PIPE_PACKET_SIZE _ 16r1120.
	CL_PIPE_MAX_PACKETS _ 16r1121.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables31
	"Auto-generated initialization of OpenCL constants for 'cl_addressing_mode'"

	CL_ADDRESS_NONE _ 16r1130.
	CL_ADDRESS_CLAMP_TO_EDGE _ 16r1131.
	CL_ADDRESS_CLAMP _ 16r1132.
	CL_ADDRESS_REPEAT _ 16r1133.
	CL_ADDRESS_MIRRORED_REPEAT _ 16r1134.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables32
	"Auto-generated initialization of OpenCL constants for 'cl_filter_mode'"

	CL_FILTER_NEAREST _ 16r1140.
	CL_FILTER_LINEAR _ 16r1141.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables33
	"Auto-generated initialization of OpenCL constants for 'cl_sampler_info'"

	CL_SAMPLER_REFERENCE_COUNT _ 16r1150.
	CL_SAMPLER_CONTEXT _ 16r1151.
	CL_SAMPLER_NORMALIZED_COORDS _ 16r1152.
	CL_SAMPLER_ADDRESSING_MODE _ 16r1153.
	CL_SAMPLER_FILTER_MODE _ 16r1154.
	CL_SAMPLER_MIP_FILTER_MODE _ 16r1155.
	CL_SAMPLER_LOD_MIN _ 16r1156.
	CL_SAMPLER_LOD_MAX _ 16r1157.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables34
	"Auto-generated initialization of OpenCL constants for 'cl_map_flags - bitfield'"

	CL_MAP_READ _ 1 bitShift: 0.
	CL_MAP_WRITE _ 1 bitShift: 1.
	CL_MAP_WRITE_INVALIDATE_REGION _ 1 bitShift: 2.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables35
	"Auto-generated initialization of OpenCL constants for 'cl_program_info'"

	CL_PROGRAM_REFERENCE_COUNT _ 16r1160.
	CL_PROGRAM_CONTEXT _ 16r1161.
	CL_PROGRAM_NUM_DEVICES _ 16r1162.
	CL_PROGRAM_DEVICES _ 16r1163.
	CL_PROGRAM_SOURCE _ 16r1164.
	CL_PROGRAM_BINARY_SIZES _ 16r1165.
	CL_PROGRAM_BINARIES _ 16r1166.
	CL_PROGRAM_NUM_KERNELS _ 16r1167.
	CL_PROGRAM_KERNEL_NAMES _ 16r1168.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables36
	"Auto-generated initialization of OpenCL constants for 'cl_program_build_info'"

	CL_PROGRAM_BUILD_STATUS _ 16r1181.
	CL_PROGRAM_BUILD_OPTIONS _ 16r1182.
	CL_PROGRAM_BUILD_LOG _ 16r1183.
	CL_PROGRAM_BINARY_TYPE _ 16r1184.
	CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE _ 16r1185.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables37
	"Auto-generated initialization of OpenCL constants for 'cl_program_binary_type'"

	CL_PROGRAM_BINARY_TYPE_NONE _ 16r0.
	CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT _ 16r1.
	CL_PROGRAM_BINARY_TYPE_LIBRARY _ 16r2.
	CL_PROGRAM_BINARY_TYPE_EXECUTABLE _ 16r4.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables38
	"Auto-generated initialization of OpenCL constants for 'cl_build_status'"

	CL_BUILD_SUCCESS _ 0.
	CL_BUILD_NONE _ -1.
	CL_BUILD_ERROR _ -2.
	CL_BUILD_IN_PROGRESS _ -3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables39
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_info'"

	CL_KERNEL_FUNCTION_NAME _ 16r1190.
	CL_KERNEL_NUM_ARGS _ 16r1191.
	CL_KERNEL_REFERENCE_COUNT _ 16r1192.
	CL_KERNEL_CONTEXT _ 16r1193.
	CL_KERNEL_PROGRAM _ 16r1194.
	CL_KERNEL_ATTRIBUTES _ 16r1195.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables40
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_arg_info'"

	CL_KERNEL_ARG_ADDRESS_QUALIFIER _ 16r1196.
	CL_KERNEL_ARG_ACCESS_QUALIFIER _ 16r1197.
	CL_KERNEL_ARG_TYPE_NAME _ 16r1198.
	CL_KERNEL_ARG_TYPE_QUALIFIER _ 16r1199.
	CL_KERNEL_ARG_NAME _ 16r119A.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables41
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_arg_address_qualifier'"

	CL_KERNEL_ARG_ADDRESS_GLOBAL _ 16r119B.
	CL_KERNEL_ARG_ADDRESS_LOCAL _ 16r119C.
	CL_KERNEL_ARG_ADDRESS_CONSTANT _ 16r119D.
	CL_KERNEL_ARG_ADDRESS_PRIVATE _ 16r119E.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables42
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_arg_access_qualifier'"

	CL_KERNEL_ARG_ACCESS_READ_ONLY _ 16r11A0.
	CL_KERNEL_ARG_ACCESS_WRITE_ONLY _ 16r11A1.
	CL_KERNEL_ARG_ACCESS_READ_WRITE _ 16r11A2.
	CL_KERNEL_ARG_ACCESS_NONE _ 16r11A3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables43
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_arg_type_qualifer'"

	CL_KERNEL_ARG_TYPE_NONE _ 0.
	CL_KERNEL_ARG_TYPE_CONST _ 1 bitShift: 0.
	CL_KERNEL_ARG_TYPE_RESTRICT _ 1 bitShift: 1.
	CL_KERNEL_ARG_TYPE_VOLATILE _ 1 bitShift: 2.
	CL_KERNEL_ARG_TYPE_PIPE _ 1 bitShift: 3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables44
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_work_group_info'"

	CL_KERNEL_WORK_GROUP_SIZE _ 16r11B0.
	CL_KERNEL_COMPILE_WORK_GROUP_SIZE _ 16r11B1.
	CL_KERNEL_LOCAL_MEM_SIZE _ 16r11B2.
	CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE _ 16r11B3.
	CL_KERNEL_PRIVATE_MEM_SIZE _ 16r11B4.
	CL_KERNEL_GLOBAL_WORK_SIZE _ 16r11B5.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables45
	"Auto-generated initialization of OpenCL constants for 'cl_kernel_exec_info'"

	CL_KERNEL_EXEC_INFO_SVM_PTRS _ 16r11B6.
	CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM _ 16r11B7.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables46
	"Auto-generated initialization of OpenCL constants for 'cl_event_info'"

	CL_EVENT_COMMAND_QUEUE _ 16r11D0.
	CL_EVENT_COMMAND_TYPE _ 16r11D1.
	CL_EVENT_REFERENCE_COUNT _ 16r11D2.
	CL_EVENT_COMMAND_EXECUTION_STATUS _ 16r11D3.
	CL_EVENT_CONTEXT _ 16r11D4.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables47
	"Auto-generated initialization of OpenCL constants for 'cl_command_type'"

	CL_COMMAND_NDRANGE_KERNEL _ 16r11F0.
	CL_COMMAND_TASK _ 16r11F1.
	CL_COMMAND_NATIVE_KERNEL _ 16r11F2.
	CL_COMMAND_READ_BUFFER _ 16r11F3.
	CL_COMMAND_WRITE_BUFFER _ 16r11F4.
	CL_COMMAND_COPY_BUFFER _ 16r11F5.
	CL_COMMAND_READ_IMAGE _ 16r11F6.
	CL_COMMAND_WRITE_IMAGE _ 16r11F7.
	CL_COMMAND_COPY_IMAGE _ 16r11F8.
	CL_COMMAND_COPY_IMAGE_TO_BUFFER _ 16r11F9.
	CL_COMMAND_COPY_BUFFER_TO_IMAGE _ 16r11FA.
	CL_COMMAND_MAP_BUFFER _ 16r11FB.
	CL_COMMAND_MAP_IMAGE _ 16r11FC.
	CL_COMMAND_UNMAP_MEM_OBJECT _ 16r11FD.
	CL_COMMAND_MARKER _ 16r11FE.
	CL_COMMAND_ACQUIRE_GL_OBJECTS _ 16r11FF.
	CL_COMMAND_RELEASE_GL_OBJECTS _ 16r1200.
	CL_COMMAND_READ_BUFFER_RECT _ 16r1201.
	CL_COMMAND_WRITE_BUFFER_RECT _ 16r1202.
	CL_COMMAND_COPY_BUFFER_RECT _ 16r1203.
	CL_COMMAND_USER _ 16r1204.
	CL_COMMAND_BARRIER _ 16r1205.
	CL_COMMAND_MIGRATE_MEM_OBJECTS _ 16r1206.
	CL_COMMAND_FILL_BUFFER _ 16r1207.
	CL_COMMAND_FILL_IMAGE _ 16r1208.
	CL_COMMAND_SVM_FREE _ 16r1209.
	CL_COMMAND_SVM_MEMCPY _ 16r120A.
	CL_COMMAND_SVM_MEMFILL _ 16r120B.
	CL_COMMAND_SVM_MAP _ 16r120C.
	CL_COMMAND_SVM_UNMAP _ 16r120D.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables48
	"Auto-generated initialization of OpenCL constants for 'command execution status'"

	CL_COMPLETE _ 16r0.
	CL_RUNNING _ 16r1.
	CL_SUBMITTED _ 16r2.
	CL_QUEUED _ 16r3.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables49
	"Auto-generated initialization of OpenCL constants for 'cl_buffer_create_type'"

	CL_BUFFER_CREATE_TYPE_REGION _ 16r1220.
! !

!OpenCLConstants class methodsFor: 'class initialization'!
initializeClassVariables50
	"Auto-generated initialization of OpenCL constants for 'cl_profiling_info'"

	CL_PROFILING_COMMAND_QUEUED _ 16r1280.
	CL_PROFILING_COMMAND_SUBMIT _ 16r1281.
	CL_PROFILING_COMMAND_START _ 16r1282.
	CL_PROFILING_COMMAND_END _ 16r1283.
	CL_PROFILING_COMMAND_COMPLETE _ 16r1284.
! !

!OpenCLError methodsFor: 'accessing' stamp: 'jcg 1/10/2010 01:53'!
code
	^errorCode! !

!OpenCLError methodsFor: 'accessing' stamp: 'jcg 1/10/2010 01:53'!
code: errorCodeInteger
	errorCode := errorCodeInteger! !

!OpenCLError class methodsFor: 'signalling' stamp: 'jmv 9/9/2015 11:34'!
check: errCode
	errCode == CL_SUCCESS ifTrue: [^self "no problemo"].
	self signalCode: errCode
	! !

!OpenCLError class methodsFor: 'signalling' stamp: 'jmv 9/9/2015 11:35'!
check: errCode text: failureString
	"If successful, do nothing.  Otherwise, signal an error with the specified failure-string."
	errCode == CL_SUCCESS ifTrue: [^self "no problemo"].
	self signal: failureString code: errCode
	! !

!OpenCLError class methodsFor: 'signalling' stamp: 'jmv 11/14/2017 14:12:36'!
errorStringForCode: errCode
	| errorNames |
	errorNames := #(
		'CLDeviceNotFound'
		'CLDeviceNotAvailable'
		'CLCompilerNotAvailable'
		'CLMemObjectAllocationFailure'
		'CLOutOfResources'
		'CLOutOfHostMemory'
		'CLProfilingInfoNotAvailable'
		'CLMemCopyOverlap'
		'CLImageFormatMismatch'
		'CLImageFormatNotSupported'
		'CLBuildProgramFailure'
		'CLMapFailure'
		'CL_MISALIGNED_SUB_BUFFER_OFFSET'
		'CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST'
		'CL_COMPILE_PROGRAM_FAILURE'
		'CL_LINKER_NOT_AVAILABLE'
		'CL_LINK_PROGRAM_FAILURE'
		'CL_DEVICE_PARTITION_FAILED'
		'CL_KERNEL_ARG_INFO_NOT_AVAILABLE'
		nil  "enough nils to fill indices 21-29"
		nil
		nil
		nil 
		nil
		nil
		nil
		nil 
		nil
		nil
		'CLInvalidValue'
		'CLInvalidDeviceType'
		'CLInvalidPlatform'
		'CLInvalidDevice'
		'CLInvalidContext'
		'CLInvalidQueueProperties'
		'CLInvalidCommandQueue'
		'CLInvalidHostPtr'
		'CLInvalidMemObject'
		'CLInvalidImageFormatDescriptor'
		'CLInvalidImageSize'
		'CLInvalidSampler'
		'CLInvalidBinary'
		'CLInvalidBuildOptions'
		'CLInvalidProgram'
		'CLInvalidProgramExecutable'
		'CLInvalidKernelName'
		'CLInvalidKernelDefinition'
		'CLInvalidKernel'
		'CLInvalidArgIndex'
		'CLInvalidArgValue'
		'CLInvalidArgSize'
		'CLInvalidKernelArgs'
		'CLInvalidWorkDimension'
		'CLInvalidWorkGroupSize'
		'CLInvalidWorkItemSize'
		'CLInvalidGlobalOffset'
		'CLInvalidEventWaitList'
		'CLInvalidEvent'
		'CLInvalidOperation'
		'CLInvalidGlObject'
		'CLInvalidBufferSize'
		'CLInvalidMipLevel'
		'CLInvalidGlobalWorkSize'
	).
	^[errorNames at: errCode negated] on: Error do: ['INVALID ERROR CODE (', errCode printString, ')!!!!!!'].! !

!OpenCLError class methodsFor: 'signalling' stamp: 'jmv 9/9/2015 11:35'!
signal: signalerText code: errCode
	errCode == CL_SUCCESS ifTrue: [^self "no problemo"].
	^self new 
			code: errCode; 
			signal: (self errorStringForCode: errCode), ': ', signalerText! !

!OpenCLError class methodsFor: 'signalling' stamp: 'jcg 1/13/2010 22:52'!
signalCode: errCode
	^self new code: errCode; signal: (self errorStringForCode: errCode)! !

!OpenCLJuliaSetMorph methodsFor: 'private' stamp: 'jcg 5/1/2010 01:57'!
changeIfDirty
	"If the Julia set needs to be redrawn, then so does this morph."
	julia isDirty ifTrue: [self changed].! !

!OpenCLJuliaSetMorph methodsFor: 'private' stamp: 'jcg 5/1/2010 11:32'!
initializeJuliaIfNecessary
	"If the OpenCL context is absent or destroyed, we need to re-initialize 'julia'."
	(julia context isNil or: [julia context isDestroyed]) 
		ifTrue: [julia initialize.
				self image: julia outputForm]! !

!OpenCLJuliaSetMorph methodsFor: 'drawing' stamp: 'jcg 5/1/2010 10:52'!
drawOn: aCanvas
	self initializeJuliaIfNecessary.
	julia isDirty ifTrue: [julia runKernel].
	super drawOn: aCanvas! !

!OpenCLJuliaSetMorph methodsFor: 'geometry' stamp: 'jcg 5/1/2010 01:28'!
extent: aPoint
	julia extent: aPoint.
	self image: julia outputForm.
	julia runKernel.
	self changed.
	! !

!OpenCLJuliaSetMorph methodsFor: 'accessing' stamp: 'jcg 5/1/2010 01:51'!
imaginary
	"Answer the 'imaginary' parameter of the Julia set."
	^julia imaginary! !

!OpenCLJuliaSetMorph methodsFor: 'accessing' stamp: 'jcg 5/1/2010 01:55'!
imaginary: newImaginary
	"Set the 'imaginary' parameter of the Julia set."
	julia imaginary: newImaginary.
	self changeIfDirty.! !

!OpenCLJuliaSetMorph methodsFor: 'accessing' stamp: 'jcg 5/1/2010 01:51'!
real
	"Answer the 'real' parameter of the Julia set."
	^julia real! !

!OpenCLJuliaSetMorph methodsFor: 'accessing' stamp: 'jcg 5/1/2010 13:45'!
real: newReal
	"Set the 'real' parameter of the Julia set."
	julia real: newReal.
	self changeIfDirty.! !

!OpenCLJuliaSetMorph methodsFor: 'initialize' stamp: 'jcg 5/1/2010 01:26'!
initialize
	super initialize.
	julia := OpenCLJuliaSet new.
	self useRainbowToneMap.
	self extent: 512@512.
	julia runKernel.
	self changed.
	
! !

!OpenCLJuliaSetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2015 17:26'!
isOrthoRectangularMorph
	^false! !

!OpenCLJuliaSetMorph methodsFor: 'tone-maps' stamp: 'jmv 8/5/2015 17:27'!
useRainbowToneMap
	"Generate a tone-map that cycles through the color-wheel.."
	self initializeJuliaIfNecessary.
	julia useRainbowToneMap! !

!CLBufferRegionStruct methodsFor: 'accessing'!
origin
	"This method was automatically generated. See CLBufferRegionStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 1! !

!CLBufferRegionStruct methodsFor: 'accessing'!
origin: anObject
	"This method was automatically generated. See CLBufferRegionStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 1 put: anObject! !

!CLBufferRegionStruct methodsFor: 'accessing'!
size
	"This method was automatically generated. See CLBufferRegionStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 9! !

!CLBufferRegionStruct methodsFor: 'accessing'!
size: anObject
	"This method was automatically generated. See CLBufferRegionStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 9 put: anObject! !

!CLBufferRegionStruct class methodsFor: 'accessing' stamp: 'jmv 12/26/2016 15:10:41'!
fields
	"
	CLBufferRegionStruct defineFields
	"
	^#(
		(origin 'size_t')
		(size 'size_t')
	)! !

!CLBufferRegionStruct class methodsFor: 'class initialization' stamp: 'jmv 12/26/2016 17:02:41'!
initialize
	"Different in 32 and 64 bits systems"
	CLBufferRegionStruct defineFields! !

!CLImageDescStruct methodsFor: 'accessing'!
buffer
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongAt: 65! !

!CLImageDescStruct methodsFor: 'accessing'!
buffer: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongAt: 65 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_array_size
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 33! !

!CLImageDescStruct methodsFor: 'accessing'!
image_array_size: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 33 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_depth
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 25! !

!CLImageDescStruct methodsFor: 'accessing'!
image_depth: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 25 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_height
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 17! !

!CLImageDescStruct methodsFor: 'accessing'!
image_height: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 17 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_row_pitch
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 41! !

!CLImageDescStruct methodsFor: 'accessing'!
image_row_pitch: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 41 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_slice_pitch
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 49! !

!CLImageDescStruct methodsFor: 'accessing'!
image_slice_pitch: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 49 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_type
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 1! !

!CLImageDescStruct methodsFor: 'accessing'!
image_type: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 1 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
image_width
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongLongAt: 9! !

!CLImageDescStruct methodsFor: 'accessing'!
image_width: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongLongAt: 9 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 14:57'!
mem_object
	"In OpenCL 1.2, this field does not exist. At this position there is field 'buffer'
	In OpenCL 2.0, a union of 'buffer' and 'mem_object', both of type cl_mem"
	^handle unsignedLongAt: 37! !

!CLImageDescStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 14:57'!
mem_object: aMemObjectId
	"In OpenCL 1.2, this field does not exist. At this position there is field 'buffer'
	In OpenCL 2.0, a union of 'buffer' and 'mem_object', both of type cl_mem"
	handle unsignedLongAt: 37 put: aMemObjectId! !

!CLImageDescStruct methodsFor: 'accessing'!
num_mip_levels
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongAt: 57! !

!CLImageDescStruct methodsFor: 'accessing'!
num_mip_levels: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongAt: 57 put: anObject! !

!CLImageDescStruct methodsFor: 'accessing'!
num_samples
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	^handle unsignedLongAt: 61! !

!CLImageDescStruct methodsFor: 'accessing'!
num_samples: anObject
	"This method was automatically generated. See CLImageDescStruct class>>fields."
	<generated>
	handle unsignedLongAt: 61 put: anObject! !

!CLImageDescStruct class methodsFor: 'accessing' stamp: 'jmv 12/26/2016 16:11:09'!
fields
	"
	CLImageDescStruct defineFields
	"
	^#(
		(image_type 'size_t') "Should actually be ulong. This is a hack: if size_8 is 8 bytes, aling next field (Assumes little endian!!)"
		(image_width 'size_t')
		(image_height 'size_t')
		(image_depth 'size_t')
		(image_array_size 'size_t')
		(image_row_pitch 'size_t')
		(image_slice_pitch 'size_t')
		(num_mip_levels 'ulong')
		(num_samples 'ulong')
		(buffer 'ulong')
	)! !

!CLImageDescStruct class methodsFor: 'utility' stamp: 'jmv 10/29/2015 15:15'!
image_type_String: image_type "a image_type constant"
	"Answer a descriptive string for the given constant."
	^image_type caseOf: {
		[CL_MEM_OBJECT_BUFFER]				->	['CL_MEM_OBJECT_BUFFER'].
		[CL_MEM_OBJECT_IMAGE2D]				-> 	['CL_MEM_OBJECT_IMAGE2D'].
		[CL_MEM_OBJECT_IMAGE3D]				->	['CL_MEM_OBJECT_IMAGE3D'].
		[CL_MEM_OBJECT_IMAGE2D_ARRAY]		->	['CL_MEM_OBJECT_IMAGE2D_ARRAY'].
		[CL_MEM_OBJECT_IMAGE1D]				->	['CL_MEM_OBJECT_IMAGE1D'].
		[CL_MEM_OBJECT_IMAGE1D_ARRAY]		->	['CL_MEM_OBJECT_IMAGE1D_ARRAY'].
		[CL_MEM_OBJECT_IMAGE1D_BUFFER]	->	['CL_MEM_OBJECT_IMAGE1D_BUFFER'].
		[CL_MEM_OBJECT_PIPE]					->	['CL_MEM_OBJECT_PIPE'].
	} otherwise: [
		String streamContents: [:s |
			s nextPutAll: '<unknown image_type '.
			image_type printOn: s base: 16.
			s nextPut: $>
		]]! !

!CLImageDescStruct class methodsFor: 'class initialization' stamp: 'jmv 12/26/2016 17:03:09'!
initialize
	"Different in 32 and 64 bits systems"
	CLImageDescStruct defineFields! !

!CLImageFormatStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 16:44'!
image_channel_data_type
	"This method was automatically generated"
	^handle unsignedLongAt: 5! !

!CLImageFormatStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 16:44'!
image_channel_data_type: anObject
	"This method was automatically generated"
	handle unsignedLongAt: 5 put: anObject! !

!CLImageFormatStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 16:44'!
image_channel_order
	"This method was automatically generated"
	^handle unsignedLongAt: 1! !

!CLImageFormatStruct methodsFor: 'accessing' stamp: 'jmv 10/29/2015 16:44'!
image_channel_order: anObject
	"This method was automatically generated"
	handle unsignedLongAt: 1 put: anObject! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 12/2/2015 12:39'!
alpha8
	^self order: CL_A type: CL_UNSIGNED_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 12/2/2015 12:39'!
alpha8Norm
	^self order: CL_A type: CL_UNORM_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:53'!
argb8888
	^self order: CL_ARGB type: CL_UNSIGNED_INT8
! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:53'!
argb8888norm
	^self order: CL_ARGB type: CL_UNORM_INT8
! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:52'!
bgra8888
	^self order: CL_BGRA type: CL_UNSIGNED_INT8
! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:53'!
bgra8888norm
	^self order: CL_BGRA type: CL_UNORM_INT8
! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 3/31/2016 11:30'!
intensity16SignedNorm
	^self order: CL_INTENSITY type: CL_SNORM_INT16! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:33'!
intensity8
	^self order: CL_INTENSITY type: CL_UNSIGNED_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:34'!
intensity8Norm
	^self order: CL_INTENSITY type: CL_UNORM_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:34'!
intensityFloat
	^self order: CL_INTENSITY type: CL_FLOAT! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:34'!
luminance8
	^self order: CL_LUMINANCE type: CL_UNSIGNED_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:34'!
luminance8Float
	^self order: CL_LUMINANCE type: CL_FLOAT! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:34'!
luminance8Norm
	^self order: CL_LUMINANCE type: CL_UNORM_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 10/29/2015 14:50'!
order: imageChannelOrder type: imageChannelDataType
	^self new
		image_channel_order: imageChannelOrder;
		image_channel_data_type: imageChannelDataType.! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 3/31/2016 10:57'!
r16Signed
	^self order: CL_R type: CL_SIGNED_INT16! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 3/31/2016 10:57'!
r16SignedNorm
	^self order: CL_R type: CL_SNORM_INT16! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 14:35'!
r8
	^self order: CL_R type: CL_UNSIGNED_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 12/2/2015 12:37'!
r8Norm
	^self order: CL_R type: CL_UNORM_INT8! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 3/31/2016 10:56'!
rFloat
	^self order: CL_R type: CL_FLOAT! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:52'!
rgba8888
	^self order: CL_RGBA type: CL_UNSIGNED_INT8
! !

!CLImageFormatStruct class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 11:53'!
rgba8888norm
	^self order: CL_RGBA type: CL_UNORM_INT8
! !

!CLImageFormatStruct class methodsFor: 'field definition' stamp: 'jmv 10/29/2015 14:52'!
fields
	"CLImageFormatStruct defineFields"
	^#(
		(image_channel_order 'ulong')
		(image_channel_data_type 'ulong')
	)! !

!CLImageFormatStruct class methodsFor: 'utility' stamp: 'jmv 10/29/2015 15:11'!
image_channel_data_type_String: channelDataType "a cl_channel_type"
	"Answer a descriptive string for the given constant."
	^channelDataType caseOf: {
		[CL_SNORM_INT8]			->	['CL_SNORM_INT8'].
		[CL_SNORM_INT16]			-> 	['CL_SNORM_INT16'].
		[CL_UNORM_INT8]			->	['CL_UNORM_INT8'].
		[CL_UNORM_INT16]			->	['CL_UNORM_INT16'].
		[CL_UNORM_SHORT_565]	->	['CL_UNORM_SHORT_565'].
		[CL_UNORM_SHORT_555]	->	['CL_UNORM_SHORT_555'].
		[CL_UNORM_INT_101010]	->	['CL_UNORM_INT_101010'].
		[CL_SIGNED_INT8]			->	['CL_SIGNED_INT8'].
		[CL_SIGNED_INT16]			->	['CL_SIGNED_INT16'].
		[CL_SIGNED_INT32]		->	['CL_SIGNED_INT32'].
		[CL_UNSIGNED_INT8]		->	['CL_UNSIGNED_INT8'].
		[CL_UNSIGNED_INT16]		->	['CL_UNSIGNED_INT16'].
		[CL_UNSIGNED_INT32]		->	['CL_UNSIGNED_INT32'].
		[CL_HALF_FLOAT]			->	['CL_HALF_FLOAT'].
		[CL_FLOAT]					->	['CL_FLOAT'].
		[CL_UNORM_INT24]			->	['CL_UNORM_INT24'].
	} otherwise: [
		String streamContents: [:s |
			s nextPutAll: '<unknown channel data-type '.
			channelDataType printOn: s base: 16.
			s nextPut: $>
		]]! !

!CLImageFormatStruct class methodsFor: 'utility' stamp: 'jmv 10/29/2015 15:10'!
image_channel_order_String: channelOrderConstant "a cl_channel_order_constant"
	"Answer a descriptive string for the given constant."
	^channelOrderConstant caseOf: {
		[CL_R]						->	['CL_R'].
		[CL_A]						-> 	['CL_A'].
		[CL_RG]					->	['CL_RG'].
		[CL_RA]					->	['CL_RA'].
		[CL_RGB]					->	['CL_RGB'].
		[CL_RGBA]				->	['CL_RGBA'].
		[CL_BGRA]				->	['CL_BGRA'].
		[CL_ARGB]				->	['CL_ARGB'].
		[CL_INTENSITY]			->	['CL_INTENSITY'].
		[CL_LUMINANCE]			->	['CL_LUMINANCE'].
		[CL_Rx]					->	['CL_Rx'].
		[CL_RGx]					-> 	['CL_RGx'].
		[CL_RGBx]				->	['CL_RGBx'].
		[CL_DEPTH]				->	['CL_DEPTH'].
		[CL_DEPTH_STENCIL]	->	['CL_DEPTH_STENCIL'].
		[CL_sRGB]				->	['CL_sRGB'].
		[CL_sRGBx]				->	['CL_sRGBx'].
		[CL_sRGBA]				->	['CL_sRGBA'].
		[CL_sBGRA]				->	['CL_sBGRA'].
		[CL_ABGR]				->	['CL_ABGR'].
	} otherwise: [
		String streamContents: [:s |
			s nextPutAll: '<unknown channel order '.
			channelOrderConstant printOn: s base: 16.
			s nextPut: $>
		]]! !

!OpenCLOpaqueHandle methodsFor: 'retain/release' stamp: 'jcg 2/18/2010 02:48'!
release
	self subclassResponsibility! !

!OpenCLOpaqueHandle methodsFor: 'retain/release' stamp: 'jcg 2/18/2010 02:48'!
retain
	self subclassResponsibility! !

!OpenCLOpaqueHandle class methodsFor: 'field definition' stamp: 'jcg 1/5/2010 11:45'!
fields
	"OpenCLOpaqueHandle allSubclassesDo: [:cls | cls defineFields]"
	^#( nil 'void*' )! !

!CLCommandQueue methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:57'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseCommandQueue__cl_command_queue: self)! !

!CLCommandQueue methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:57'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainCommandQueue__cl_command_queue: self)! !

!CLContext methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseContext__cl_context: self)! !

!CLContext methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainContext__cl_context: self)! !

!CLEvent methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseEvent__cl_event: self)! !

!CLEvent methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainEvent__cl_event: self)! !

!CLKernel methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseKernel__cl_kernel: self)! !

!CLKernel methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:58'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainKernel__cl_kernel: self)! !

!CLMem methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:59'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseMemObject__cl_mem: self)! !

!CLMem methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 11:59'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainMemObject__cl_mem: self)! !

!CLProgram methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 12:11'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseProgram__cl_program: self)! !

!CLProgram methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 12:10'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainProgram__cl_program: self)! !

!CLSampler methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 12:11'!
release
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clReleaseSampler__cl_sampler: self)! !

!CLSampler methodsFor: 'retain/release' stamp: 'jmv 9/9/2015 12:11'!
retain
	"Called only by OpenCLRefCount."
	OpenCLError check: (OpenCLPrimitiveInterface current clRetainSampler__cl_sampler: self)! !

!OpenCLObject methodsFor: 'converting' stamp: 'jcg 2/18/2010 03:03'!
asOpenCLArg
	^id asOpenCLArg! !

!OpenCLObject methodsFor: 'accessing' stamp: 'jcg 1/13/2010 01:07'!
id
	"Answer a reference to the OpenCLOpaqueHandle sub-instance that can be passed directly as an FFI argument."
	^id! !

!OpenCLObject methodsFor: 'initialize' stamp: 'jcg 1/13/2010 01:06'!
id: anOpenCLOpaqueHandle
	id := anOpenCLOpaqueHandle! !

!OpenCLObject methodsFor: 'initialize' stamp: 'jcg 6/7/2010 01:37'!
invalidateOnStartUp
	"Squeak is starting up, so release our ID... it's invalid anyway, and would cause crashes if we tried to use it."
	id := nil.! !

!OpenCLObject methodsFor: 'testing' stamp: 'jcg 5/1/2010 09:38'!
isDestroyed
	^id isNil! !

!OpenCLObject class methodsFor: 'class initialization' stamp: 'jcg 6/7/2010 01:07'!
initialize
	"This functionality is now consolidated in CL class."
	"OpenCLObject initialize"
	Smalltalk removeFromStartUpList: self.! !

!OpenCLObject class methodsFor: 'instance creation' stamp: 'jcg 1/14/2010 00:03'!
new
	self shouldNotImplement! !

!OpenCLObject class methodsFor: 'private' stamp: 'jcg 1/14/2010 00:03'!
wrappingHandle: anOpenCLOpaqueHandle "of the appropriate subclass"
	"Users of the OpenCL framework should never call this directly."
	^self basicNew
		initialize;
		id: anOpenCLOpaqueHandle;
		yourself! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
addressBits
	"Get the CL_DEVICE_ADDRESS_BITS property: the default compute device address space size specified as an unsigned integer value in bits. Currently supported values are 32 or 64 bits."
	| buf |
	buf := IntegerArray new: 1.
	self getDeviceInfoParameter: CL_DEVICE_ADDRESS_BITS into: buf.
	^buf first! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
canExecuteKernel
	^((self bitfieldProperty: CL_DEVICE_EXECUTION_CAPABILITIES) bitAnd: CL_EXEC_KERNEL) > 0! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
canExecuteNativeKernel
	^((self bitfieldProperty: CL_DEVICE_EXECUTION_CAPABILITIES) bitAnd: CL_EXEC_NATIVE_KERNEL) > 0! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
canQueueEnableProfiling
	^(self queueProperties bitAnd: CL_QUEUE_PROFILING_ENABLE) > 0
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
canQueueOutOfOrder
	^(self queueProperties bitAnd: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE) > 0
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jcg 1/11/2010 00:56'!
doublePrecisionConfig
	self notYetImplemented. "CLDeviceDoubleFpConfig is in an extension header file"
	"^self floatingPointPrecisionConfig: CLDeviceDoubleFpConfig"
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:49'!
driverVersion
	"OpenCL software driver version string in the form major_number.minor_number."
	^self stringProperty: CL_DRIVER_VERSION! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/5/2016 17:12:09'!
extensions
	^extensions ifNil: [
		| buf size |
		buf := String new: 10000.
		size := self getDeviceInfoParameter: CL_DEVICE_EXTENSIONS into: buf.
		(buf at: size) numericValue = 0 ifTrue: [size := size - 1 "strip off terminal NULL"].
		extensions := (buf first: size) findTokens: Character space.
	].
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:49'!
globalMemCacheSize
	"Size of global memory cache in bytes."
	^self ulongProperty: CL_DEVICE_GLOBAL_MEM_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
globalMemCacheType
	"Type of global memory cache supported. Valid values are: CL_NONE, CL_READ_ONLY_CACHE, and CL_READ_WRITE_CACHE."
	^self uintProperty: CL_DEVICE_GLOBAL_MEM_CACHE_TYPE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
globalMemCachelineSize
	"Size of global memory cache line in bytes."
	^self uintProperty: CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
	! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
globalMemSize
	"Size of global device memory in bytes"
	^self ulongProperty: CL_DEVICE_GLOBAL_MEM_SIZE
	! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jcg 1/11/2010 00:56'!
halfPrecisionConfig
	self notYetImplemented. "CLDeviceHalfFpConfig is in an extension header file"
	"^self floatingPointPrecisionConfig: CLDeviceHalfFpConfig"
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
hasImageSupport
	"Is CL_TRUE if images are supported by the OpenCL device and CL_FALSE otherwise."
	^self booleanProperty: CL_DEVICE_IMAGE_SUPPORT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
image2dMaxHeight
	"Max height of 2D image in pixels. The minimum value is 8192 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self sizetProperty: CL_DEVICE_IMAGE2D_MAX_HEIGHT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
image2dMaxWidth
	"Max width of 2D image in pixels. The minimum value is 8192 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self sizetProperty: CL_DEVICE_IMAGE2D_MAX_WIDTH! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
image3dMaxDepth
	"Max depth of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self sizetProperty: CL_DEVICE_IMAGE3D_MAX_DEPTH! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
image3dMaxHeight
	"Max height of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self sizetProperty: CL_DEVICE_IMAGE3D_MAX_HEIGHT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
image3dMaxWidth
	"Max width of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self sizetProperty: CL_DEVICE_IMAGE3D_MAX_WIDTH! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
isAvailable
	"Get the CL_DEVICE_AVAILABLE property:  'true' if the device is available and 'false' if the device is not available."
	^self booleanProperty: CL_DEVICE_AVAILABLE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
isCompilerAvailable
	"Get the CL_DEVICE_COMPILER_AVAILABLE property:  Is 'false' if the implementation does not have a compiler available to compile the program source. Is 'true' if the compiler is available. This can be 'false' for the embededed platform profile only."
	^self booleanProperty: CL_DEVICE_COMPILER_AVAILABLE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
isLittleEndian
	"Get the CL_DEVICE_ENDIAN_LITTLE property:  Is 'true' if the OpenCL device is a little endian device and 'false' otherwise.."
	^self booleanProperty: CL_DEVICE_ENDIAN_LITTLE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:44'!
localMemSize
	"Size of local memory arena in bytes. The minimum value is 16 KB."
	^self ulongProperty: CL_DEVICE_LOCAL_MEM_SIZE
	! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
localMemType
	"Type of local memory supported. This can be set to CL_LOCAL implying dedicated local memory storage such as SRAM, or CL_GLOBAL."
	^self uintProperty: CL_DEVICE_LOCAL_MEM_TYPE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
maxClockFrequency
	"Maximum configured clock frequency of the device in MHz."
	^self uintProperty: CL_DEVICE_MAX_CLOCK_FREQUENCY! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:44'!
maxComputeUnits
	"The number of parallel compute cores on the OpenCL device. The minimum value is 1."
	^self uintProperty: CL_DEVICE_MAX_COMPUTE_UNITS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:39'!
maxConstantArgs
	"Max number of arguments declared with the __constant qualifier in a kernel. The minimum value is 8."
	^self uintProperty: CL_DEVICE_MAX_CONSTANT_ARGS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
maxConstantBufferSize
	"Max size in bytes of a constant buffer allocation. The minimum value is 64 KB."
	^self ulongProperty: CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:44'!
maxMemAllocSize
	"Max size of memory object allocation in bytes. The minimum value is max (1/4th of CL_DEVICE_GLOBAL_MEM_SIZE, 128*1024*1024)"
	^self ulongProperty: CL_DEVICE_MAX_MEM_ALLOC_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
maxParameterSize
	"Max size in bytes of the arguments that can be passed to a kernel. The minimum value is 256."
	^self sizetProperty: CL_DEVICE_MAX_PARAMETER_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
maxReadImageArgs
	"Max number of simultaneous image objects that can be read by a kernel. The minimum value is 128 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self uintProperty: CL_DEVICE_MAX_READ_IMAGE_ARGS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
maxSamplers
	"Maximum number of samplers that can be used in a kernel. The minimum value is 16 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE. (Also see sampler_t.)"
	^self uintProperty: CL_DEVICE_MAX_SAMPLERS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
maxWorkGroupSize
	"Maximum number of work-items in a work-group executing a kernel using the data parallel execution model. (Refer to clEnqueueNDRangeKernel). The minimum value is 1."
	^self sizetProperty: CL_DEVICE_MAX_WORK_GROUP_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
maxWorkItemDimensions
	"Maximum dimensions that specify the global and local work-item IDs used by the data parallel execution model. (Refer to clEnqueueNDRangeKernel). The minimum value is 3."
	^self uintProperty: CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 12/23/2016 17:12:53'!
maxWorkItemSizes
	"Maximum number of work-items that can be specified in each dimension of the work-group to clEnqueueNDRangeKernel.

Returns n size_t entries, where n is the value returned by the query for CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS. The minimum value is (1, 1, 1)."
	| buf | 
Smalltalk wordSize = 8 ifTrue: [
	buf := ByteArray new: 24.
	self getDeviceInfoParameter: CL_DEVICE_MAX_WORK_ITEM_SIZES into: buf.
	^#(1 9 17) 
		collect: [:i | buf unsignedLong64At: i bigEndian: Smalltalk isBigEndian].
].
	buf := ByteArray new: 12.
	self getDeviceInfoParameter: CL_DEVICE_MAX_WORK_ITEM_SIZES into: buf.
	^#(1 5 9) 
		collect: [:i | buf unsignedLongAt: i bigEndian: Smalltalk isBigEndian].
	! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
maxWriteImageArgs
	"Max number of simultaneous image objects that can be written to by a kernel. The minimum value is 8 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE."
	^self uintProperty: CL_DEVICE_MAX_WRITE_IMAGE_ARGS! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
memBaseAddrAlign
	"Describes the alignment in bits of the base address of any allocated memory object."
	^self uintProperty: CL_DEVICE_MEM_BASE_ADDR_ALIGN! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
minDataTypeAlignSize
	"The smallest alignment in bytes which can be used for any data type."
	^self uintProperty: CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jcg 1/13/2010 00:35'!
platform
	"The platform associated with this device (cached during initialization)."
	^platform
	! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
preferredVectorWidthChar
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
preferredVectorWidthDouble
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector.

If the cl_khr_fp64 extension is not supported, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE must return 0."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
preferredVectorWidthFloat
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
preferredVectorWidthInt
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
preferredVectorWidthLong
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
preferredVectorWidthShort
	"Preferred native vector width size for built-in scalar types that can be put into vectors. The vector width is defined as the number of scalar elements that can be stored in the vector."
	^self uintProperty: CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
profile
	"OpenCL profile string. Returns the profile name supported by the device (see note). The profile name returned can be one of the following strings:

FULL_PROFILE - if the device supports the OpenCL specification (functionality defined as part of the core specification and does not require any extensions to be supported).

EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile."
	^self stringProperty: CL_DEVICE_PROFILE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
profilingTimerResolution
	"Describes the resolution of device timer. This is measured in nanoseconds."
	^self sizetProperty: CL_DEVICE_PROFILING_TIMER_RESOLUTION! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
queueProperties
	"Describes the command-queue properties supported by the device. This is a bit-field that describes one or more of the following values:
	CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
	CL_QUEUE_PROFILING_ENABLE
These properties are described in the table for clCreateCommandQueue. The mandated minimum capability is CL_QUEUE_PROFILING_ENABLE."
	^self bitfieldProperty: CL_DEVICE_QUEUE_PROPERTIES! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
singlePrecisionConfig
	^self floatingPointPrecisionConfig: CL_DEVICE_SINGLE_FP_CONFIG
! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
supportsErrorCorrection
	"Get the CL_DEVICE_ERROR_CORRECTION_SUPPORT property:  Is 'true' if the device implements error correction for the memories, caches, registers etc. in the device. Is 'false' if the device does not implement error correction. This can be a requirement for certain clients of OpenCL."
	^self booleanProperty: CL_DEVICE_ERROR_CORRECTION_SUPPORT! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:45'!
type
	"The OpenCL device type. Currently supported values are one of or a combination of: CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR, or CL_DEVICE_TYPE_DEFAULT."
	^self ulongProperty: CL_DEVICE_TYPE! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:48'!
vendor
	"Vendor name string."
	^self stringProperty: CL_DEVICE_VENDOR! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:43'!
vendorID
	"A unique device vendor identifier. An example of a unique device identifier could be the PCIe ID."
	^self uintProperty: CL_DEVICE_VENDOR_ID! !

!OpenCLDevice methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
version
	"OpenCL version string. Returns the OpenCL version supported by the device. This version string has the following format:

OpenCL<space><major_version.minor_version><space><vendor-specific information>

The major_version.minor_version value returned will be 1.0."
	^self stringProperty: CL_DEVICE_VERSION! !

!OpenCLDevice methodsFor: 'initialize' stamp: 'jcg 1/13/2010 00:33'!
platform: anOpenCLPlatform
	platform := anOpenCLPlatform! !

!OpenCLDevice methodsFor: 'report' stamp: 'jmv 9/16/2015 17:09'!
reportExtensionsOn: aStream
	"
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].
	"
	aStream nextPutAll: 'Extensions: '; newLine.
	self extensions do: [ :extension | aStream nextPut: $	 ; nextPutAll: extension; newLine ]! !

!OpenCLDevice methodsFor: 'report' stamp: 'jmv 9/16/2015 17:08'!
reportGeneralOn: aStream
	"
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].
	"

	aStream nextPutAll: 'Device id: '.
	id getHandle printOn: aStream.
	aStream newLine.
	aStream nextPutAll: 'Name: '; nextPutAll: self name; newLine.
	aStream nextPutAll: 'Vendor: '; nextPutAll: self vendor; newLine.
	aStream nextPutAll: 'DriverVersion: '; nextPutAll: self driverVersion printString; newLine.
	aStream nextPutAll: 'MaxComputeUnits: '; nextPutAll: self maxComputeUnits printString; newLine.
	aStream nextPutAll: 'Version: '; nextPutAll: self version; newLine.

	aStream nextPutAll: 'AddressBits: '; nextPutAll: self addressBits printString; newLine.
	aStream nextPutAll: 'CanExecuteKernel: '; nextPutAll: self canExecuteKernel printString; newLine.
	aStream nextPutAll: 'CanExecuteNativeKernel: '; nextPutAll: self canExecuteNativeKernel printString; newLine.
	aStream nextPutAll: 'CanQueueEnableProfiling: '; nextPutAll: self canQueueEnableProfiling printString; newLine.
	aStream nextPutAll: 'CanQueueOutOfOrder: '; nextPutAll: self canQueueOutOfOrder printString; newLine.
	aStream nextPutAll: 'GlobalMemCacheSize: '; nextPutAll: self globalMemCacheSize printStringAsBytes; newLine.
	aStream nextPutAll: 'GlobalMemCacheType: '; nextPutAll: self globalMemCacheType printString; newLine.
	aStream nextPutAll: 'GlobalMemCachelineSize: '; nextPutAll: self globalMemCachelineSize printString; newLine.
	aStream nextPutAll: 'GlobalMemSize: '; nextPutAll: self globalMemSize printStringAsBytes; newLine.
	aStream nextPutAll: 'HasImageSupport: '; nextPutAll: self hasImageSupport printString; newLine.
	aStream nextPutAll: 'Image2dMaxHeight: '; nextPutAll: self image2dMaxHeight printString; newLine.
	aStream nextPutAll: 'Image2dMaxWidth: '; nextPutAll: self image2dMaxWidth printString; newLine.
	aStream nextPutAll: 'Image3dMaxDepth: '; nextPutAll: self image3dMaxDepth printString; newLine.
	aStream nextPutAll: 'Image3dMaxHeight: '; nextPutAll: self image3dMaxHeight printString; newLine.
	aStream nextPutAll: 'Image3dMaxWidth: '; nextPutAll: self image3dMaxWidth printString; newLine.
	aStream nextPutAll: 'IsAvailable: '; nextPutAll: self isAvailable printString; newLine.
	aStream nextPutAll: 'IsCompilerAvailable: '; nextPutAll: self isCompilerAvailable printString; newLine.
	aStream nextPutAll: 'IsLittleEndian: '; nextPutAll: self isLittleEndian printString; newLine.
	aStream nextPutAll: 'LocalMemSize: '; nextPutAll: self localMemSize printString; newLine.
	aStream nextPutAll: 'LocalMemType: '; nextPutAll: self localMemType printString; newLine.
	aStream nextPutAll: 'MaxClockFrequency: '; nextPutAll: self maxClockFrequency printString; newLine.
	aStream nextPutAll: 'MaxConstantArgs: '; nextPutAll: self maxConstantArgs printString; newLine.
	aStream nextPutAll: 'MaxConstantBufferSize: '; nextPutAll: self maxConstantBufferSize printStringAsBytes; newLine.
	aStream nextPutAll: 'MaxMemAllocSize: '; nextPutAll: self maxMemAllocSize printStringAsBytes; newLine.
	aStream nextPutAll: 'MaxParameterSize: '; nextPutAll: self maxParameterSize printString; newLine.
	aStream nextPutAll: 'MaxReadImageArgs: '; nextPutAll: self maxReadImageArgs printString; newLine.
	aStream nextPutAll: 'MaxSamplers: '; nextPutAll: self maxSamplers printString; newLine.
	aStream nextPutAll: 'MaxWorkGroupSize: '; nextPutAll: self maxWorkGroupSize printString; newLine.
	aStream nextPutAll: 'MaxWorkItemDimensions: '; nextPutAll: self maxWorkItemDimensions printString; newLine.
	aStream nextPutAll: 'MaxWorkItemSizes: '; nextPutAll: self maxWorkItemSizes printString; newLine.
	aStream nextPutAll: 'MaxWriteImageArgs: '; nextPutAll: self maxWriteImageArgs printString; newLine.
	aStream nextPutAll: 'MemBaseAddrAlign: '; nextPutAll: self memBaseAddrAlign printString; newLine.
	aStream nextPutAll: 'MinDataTypeAlignSize: '; nextPutAll: self minDataTypeAlignSize printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthChar: '; nextPutAll: self preferredVectorWidthChar printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthDouble: '; nextPutAll: self preferredVectorWidthDouble printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthFloat: '; nextPutAll: self preferredVectorWidthFloat printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthInt: '; nextPutAll: self preferredVectorWidthInt printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthLong: '; nextPutAll: self preferredVectorWidthLong printString; newLine.
	aStream nextPutAll: 'PreferredVectorWidthShort: '; nextPutAll: self preferredVectorWidthShort printString; newLine.
	aStream nextPutAll: 'Profile: '; nextPutAll: self profile; newLine.
	aStream nextPutAll: 'ProfilingTimerResolution: '; nextPutAll: self profilingTimerResolution printString; newLine.
	aStream nextPutAll: 'QueueProperties: '; nextPutAll: self queueProperties printString; newLine.
	aStream nextPutAll: 'SinglePrecisionConfig: '; nextPutAll: self singlePrecisionConfig printString; newLine.
	aStream nextPutAll: 'SupportsErrorCorrection: '; nextPutAll: self supportsErrorCorrection printString; newLine.
	aStream nextPutAll: 'Type: '; nextPutAll: self type printString; newLine.
	aStream nextPutAll: 'VendorID: '; nextPutAll: self vendorID printString; newLine.! !

!OpenCLDevice methodsFor: 'private' stamp: 'jcg 1/12/2010 00:22'!
bitfieldProperty: property
	"Get the specified property of type cl_bitfield (or a subtype)."
	^self ulongProperty: property! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 9/9/2015 11:48'!
booleanProperty: property
	"Get the boolean device property:  'true' if the device is available and 'false' if the device is not available."
	| buf bool |
	buf := IntegerArray new: 1.
	self getDeviceInfoParameter: property into: buf.
	bool := buf first.
	bool == CL_TRUE ifTrue: [^true].
	bool == CL_FALSE ifTrue: [^false].
	self error: 'failed to obtain boolean device property'! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 8/5/2015 17:30'!
floatingPointPrecisionConfig: precision "CLDevice[Single|Double|Half]FpConfig"
	"Get the specified property, one of CL_DEVICE_SINGLE_FP_CONFIG, CL_DEVICE_DOUBLE_FP_CONFIG, or CL_DEVICE_HALF_FP_CONFIG. property.  Answer a bit-field that describes the OPTIONAL double precision floating-point capability of the OpenCL device. This is a bit-field that describes one or more of the following values:
	CL_FP_DENORM - denorms are supported.
	CL_FP_INF_NAN - INF and NaNs are supported.
	CL_FP_ROUND_TO_NEAREST - round to nearest even rounding mode supported.
	CL_FP_ROUND_TO_ZERO - round to zero rounding mode supported.
	CL_FP_ROUND_TO_INF - round to +ve and -ve infinity rounding modes supported.
	CP_FP_FMA - IEEE754-2008 fused multiply-add is supported."
	| buf isBigEndian |
	buf := ByteArray new: 8.
	self getDeviceInfoParameter: precision into: buf.
	isBigEndian := Smalltalk isBigEndian.
	^ ((buf unsignedLongAt: 5 bigEndian: isBigEndian) bitShift: 32) +
		(buf unsignedLongAt: 1 bigEndian: isBigEndian)! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 12/20/2016 22:58:35'!
getDeviceInfoParameter: param "cl_device_info" into: buf "String, IntegerArray, etc."
	"Read the specified parameter into 'buf'.  Answer the size (in bytes) of the data stored into 'buf'."
	| result size |
	size := IntegerArray new: 1.
	result := OpenCLPrimitiveInterface current
			clGetDeviceInfo__cl_device_id: id
						cl_device_info: param
						size_t: buf byteSize
						void__ptr: buf
						size_t__ptr: size.
	result == CL_SUCCESS ifFalse: [OpenCLError signal: 'failed to get device info' code: result].
	^size first! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 12/23/2016 15:46:57'!
sizetProperty: property
	"Get the specified size_t property."
	^Smalltalk wordSize = 8
		ifTrue: [ self ulongProperty: property ]
		ifFalse: [ self uintProperty: property ]! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 9/5/2016 17:12:12'!
stringProperty: property
	"Get the char[] device property."
	| string size |
	string := String new: 1000.
	[size := self getDeviceInfoParameter: property into: string]
		on: OpenCLError do: [
			"Try again with much larger string."
			string := String new: 100000.
			size := self getDeviceInfoParameter: property into: string
		].
	(string at: size) numericValue = 0 ifTrue: [size := size - 1 "strip off terminal NULL character"].
	^string first: size! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 8/5/2015 17:29'!
uintProperty: property
	"Get the specified unsigned 32-bit property."
	| buf |
	buf := ByteArray new: 4.
	self getDeviceInfoParameter: property into: buf.
	^buf unsignedLongAt: 1 bigEndian: Smalltalk isBigEndian! !

!OpenCLDevice methodsFor: 'private' stamp: 'jmv 8/5/2015 17:30'!
ulongProperty: property
	"Get the specified unsigned 64-bit property."
	| buf isBigEndian |
	buf := ByteArray new: 8.
	self getDeviceInfoParameter: property into: buf.
	isBigEndian := Smalltalk isBigEndian.
	^ ((buf unsignedLongAt: 5 bigEndian: isBigEndian) bitShift: 32) +
		(buf unsignedLongAt: 1 bigEndian: isBigEndian)! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
allCPUs
	^self allDevicesOfType: CL_DEVICE_TYPE_CPU! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jmv 12/20/2016 22:50:27'!
allDeviceIDsOfType: deviceType
	"Answer a list of CLDeviceIDs corresponding to the available devices of the specified type."
	| resultCount bytes result handles handleSize |

	resultCount := IntegerArray new: 1.
	"OpenCL will stash the results in 'bytes'."
	handleSize := CLDeviceID byteSize.
	bytes := ByteArray new: (100 * handleSize). "100 is enough for anybody"
	result := OpenCLPrimitiveInterface current
				clGetDeviceIDs__cl_platform_id: id
							cl_device_type: deviceType
							cl_uint: 100
							cl_device_id__ptr: (CLDeviceID new setHandle: bytes)
							cl_uint__ptr: resultCount.
	result == CL_SUCCESS ifFalse: ["OpenCLError signal: 'failed to get deviceIDs' code: result" ^#()].
	resultCount := resultCount first. "don't need the IntegerArray any more."
	handles := Array new: resultCount.
	1 to: resultCount do: [:i |
		| handle |
		handle := bytes copyFrom: (i-1*handleSize+1) to: (i*handleSize).
		handles at: i put: (CLDeviceID new setHandle: handle)
	].
	^handles! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
allDevices
	^self allDevicesOfType: CL_DEVICE_TYPE_ALL! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jmv 8/5/2015 18:01'!
allDevicesOfType: deviceType
	^(self allDeviceIDsOfType: deviceType) 
		collect: [ :eachId | (OpenCLDevice wrappingHandle: eachId) platform: self]! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:47'!
allGPUs
	^self allDevicesOfType: CL_DEVICE_TYPE_GPU! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jcg 1/10/2010 02:22'!
extensions
	^extensions! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jcg 1/10/2010 02:22'!
profile
	^profile! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jcg 1/10/2010 02:22'!
vendor
	^vendor! !

!OpenCLPlatform methodsFor: 'accessing' stamp: 'jcg 1/10/2010 02:22'!
version
	^version! !

!OpenCLPlatform methodsFor: 'private' stamp: 'jmv 9/5/2016 17:12:15'!
getPlatformInfoParameter: param "cl_platform_info"
	| buf result size |
	buf := String new: 1000.
	size := IntegerArray new: 1.
	result := OpenCLPrimitiveInterface current
			clGetPlatformInfo__cl_platform_id: id 
						cl_platform_info: param 
						size_t: buf size 
						void__ptr: buf 
						size_t__ptr: size.
	result == CL_SUCCESS ifFalse: [
		"If the reason that we failed is that there wasn't enough space, try again with more.
		 Unfortunately, we aren't told the actual size, so use something really big."
		buf := String new: 100000.
		result := OpenCLPrimitiveInterface current 
			clGetPlatformInfo__cl_platform_id: id 
						cl_platform_info: param 
						size_t: buf size
						void__ptr: buf 
						size_t__ptr: size.
	].
	result == CL_SUCCESS ifFalse: [OpenCLError signalCode: result].
	size := size first. "don't need the IntegerArray any more."
	(buf at: size) numericValue = 0 ifTrue: [size := size - 1 "strip off terminal NULL character"].
	^buf first: size! !

!OpenCLPlatform methodsFor: 'initialize' stamp: 'jmv 9/9/2015 11:45'!
id: aCLPlatformID
	super id: aCLPlatformID.
	profile := self getPlatformInfoParameter: CL_PLATFORM_PROFILE.
	version := self getPlatformInfoParameter: CL_PLATFORM_VERSION.
	name := self getPlatformInfoParameter: CL_PLATFORM_NAME.
	vendor := self getPlatformInfoParameter: CL_PLATFORM_VENDOR.
	extensions := self getPlatformInfoParameter: CL_PLATFORM_EXTENSIONS.
	extensions := extensions findTokens: Character space! !

!OpenCLPlatform methodsFor: 'report' stamp: 'jmv 12/22/2016 18:12:09'!
reportOn: aStream
	"
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: StdIOWriteStream stdout ].
	"
	| context devices |
	aStream nextPutAll: '=================== '; newLine.
	aStream nextPutAll: 'Platform: '; nextPutAll: name; newLine.
	aStream nextPutAll: '=================== '; newLine.
	aStream nextPutAll: '-----GPUs---------------------------- '; newLine.
	aStream nextPutAll: '--------------------------------------- '; newLine.
	self allGPUs do: [ :gpu |
			gpu reportGeneralOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			gpu reportExtensionsOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			(OpenCLContext newForDevices: {gpu})
				reportImageFormatsOn: aStream ]
		separatedBy: [aStream nextPutAll: '--------------------------------------- '; newLine].
	aStream nextPutAll: '--------------------------------------- '; newLine.
	aStream nextPutAll: '-----CPUs---------------------------- '; newLine.
	aStream nextPutAll: '--------------------------------------- '; newLine.
	self allCPUs do: [ :cpu |
			cpu reportGeneralOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			cpu reportExtensionsOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			(OpenCLContext newForDevices: {cpu})
				reportImageFormatsOn: aStream ]
		separatedBy: [aStream nextPutAll: '--------------------------------------- '; newLine].
	aStream nextPutAll: '=================== '; newLine.
	devices _ self allDevices.
	devices notEmpty ifTrue: [
		context _ OpenCLContext newForDevices: devices.
		context reportImageFormatsOn: aStream.
		aStream nextPutAll: '=================== '; newLine ]! !

!OpenCLPlatform methodsFor: 'report' stamp: 'jmv 11/17/2016 11:06:58'!
shortReportOn: aStream
	"
	OpenCLPlatform allPlatforms do: [ :platform | platform shortReportOn: Transcript ].
	OpenCLPlatform allPlatforms do: [ :platform | platform shortReportOn: StdIOWriteStream stdout ].
	"
	aStream nextPutAll: '=================== '; newLine.
	aStream nextPutAll: 'Platform: '; nextPutAll: name; newLine.
	aStream nextPutAll: '=================== '; newLine.
	aStream nextPutAll: '-----GPUs---------------------------- '; newLine.
	aStream nextPutAll: '--------------------------------------- '; newLine.
	self allGPUs do: [ :gpu |
			gpu reportGeneralOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			gpu reportExtensionsOn: aStream ]
		separatedBy: [aStream nextPutAll: '--------------------------------------- '; newLine].
	aStream nextPutAll: '--------------------------------------- '; newLine.
	aStream nextPutAll: '-----CPUs---------------------------- '; newLine.
	aStream nextPutAll: '--------------------------------------- '; newLine.
	self allCPUs do: [ :cpu |
			cpu reportGeneralOn: aStream.
			aStream nextPutAll: '--------------------------------------- '; newLine.
			cpu reportExtensionsOn: aStream ]
		separatedBy: [aStream nextPutAll: '--------------------------------------- '; newLine].
	aStream nextPutAll: '=================== '; newLine.
	aStream flush! !

!OpenCLPlatform class methodsFor: 'private' stamp: 'jmv 12/20/2016 22:51:13'!
allPlatformIDs
	"Answer a list of CLPlatformIDs corresponding to the available OpenCL platforms."
	| resultCount bytes result handles handleSize |
	
	resultCount := IntegerArray new: 1.
	"OpenCL will stash the results in 'bytes'."
	handleSize _ CLPlatformID byteSize.
	bytes := ByteArray new: (100 * handleSize). "100 is enough for anyone"
	result := OpenCLPrimitiveInterface current
				clGetPlatformIDs__cl_uint: 100 
							cl_platform_id__ptr: (CLPlatformID new setHandle: bytes) 
							cl_uint__ptr: resultCount.
	result == CL_SUCCESS ifFalse: [OpenCLError signal: 'failed to get platformIDs' code: result].
	resultCount := resultCount first. "don't need the IntegerArray any more."
	handles := Array new: resultCount.
	1 to: resultCount do: [:i |
		| handle |
		handle := bytes copyFrom: (i-1*handleSize+1) to: (i*handleSize).
		handles at: i put: (CLPlatformID new setHandle: handle)
	].
	^handles! !

!OpenCLPlatform class methodsFor: 'instance creation' stamp: 'jcg 1/14/2010 00:09'!
allPlatforms
	"OpenCLPlatform allPlatforms"
	^self allPlatformIDs collect: [:id | self wrappingHandle: id]! !

!OpenCLPlatform class methodsFor: 'report application' stamp: 'jmv 4/25/2016 15:32'!
buildOpenCLReportApp
	"
	OpenCLPlatform buildOpenCLReportApp
	"
	AppLauncher appGlobalName: self name appSelector: #shortReportAndQuit.
	Smalltalk reduceCuis.
	Smalltalk saveAs: 'OpenCLReport' andQuit: true clearAllClassState: false! !

!OpenCLPlatform class methodsFor: 'report application' stamp: 'jmv 11/17/2016 10:50:11'!
shortReport
	"On StdIO
	OpenCLPlatform shortReport
	"
	OpenCLPlatform allPlatforms do: [ :platform |
		platform shortReportOn: StdIOWriteStream stdout ]! !

!OpenCLPlatform class methodsFor: 'report application' stamp: 'jmv 11/17/2016 10:50:19'!
shortReportAndQuit
	"On StdIO
	OpenCLPlatform shortReport
	"
	OpenCLPlatform allPlatforms do: [ :platform |
		platform shortReportOn: StdIOWriteStream stdout ].

	Smalltalk snapshot: false andQuit: true embedded: false clearAllClassState: false! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'jmv 7/17/2019 14:33:04'!
allCPUs
	"Answer some device. If possible, answer some GPU. If none is available, answer some CPU.
	If none is available, answer nil
	OpenCLPlatform allCPUs
	"
	| platforms |
	platforms := OpenCLPlatform allPlatforms.
	^ Array streamContents: [ :strm |
		platforms do: [ :platform | platform allCPUs do: [ :device | strm nextPut: device ]]]! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'jmv 7/17/2019 14:11:23'!
allDevices
	"Answer some device. If possible, answer some GPU. If none is available, answer some CPU.
	If none is available, answer nil
	OpenCLPlatform someDevice
	"
	| platforms |
	platforms := OpenCLPlatform allPlatforms.
	^ Array streamContents: [ :strm |
		platforms do: [ :platform | platform allDevices do: [ :device | strm nextPut: device ]]]! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'jmv 7/17/2019 14:21:38'!
allGPUs
	"Answer some device. If possible, answer some GPU. If none is available, answer some CPU.
	If none is available, answer nil
	OpenCLPlatform someDevice
	"
	| platforms |
	platforms := OpenCLPlatform allPlatforms.
	^ Array streamContents: [ :strm |
		platforms do: [ :platform | platform allGPUs do: [ :device | strm nextPut: device ]]]! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'jmv 4/2/2020 17:39:40'!
cpu
	self allPlatforms do: [ :platform | platform allCPUs do: [ :cpu | ^ cpu ]].
	^ nil! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'pb 6/18/2019 01:55:43'!
preferredDevice
	^ PreferredDevice ifNil: [self someDevice]! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'pb 6/18/2019 01:56:06'!
preferredDevice: anOpenCLDevice
	PreferredDevice := anOpenCLDevice ! !

!OpenCLPlatform class methodsFor: 'devices' stamp: 'jmv 12/26/2016 10:55:33'!
someDevice
	"Answer some device. If possible, answer some GPU. If none is available, answer some CPU.
	If none is available, answer nil
	OpenCLPlatform someDevice
	"
	| platforms |
	platforms := OpenCLPlatform allPlatforms.
	platforms do: [ :platform | platform allGPUs do: [ :gpu | ^ gpu ]].
	platforms do: [ :platform | platform allDevices do: [ :device | ^ device ]].
	^ nil! !

!OpenCLRefCountedObject methodsFor: 'retain/release' stamp: 'jcg 1/15/2010 01:40'!
destroy
	"Call this if you *know* that this object will no longer be used.  See class comment."
	id ifNil: [^self "either we're already destroyed, or not initialized yet."].
	[refCounter count = 0] whileFalse: [refCounter release].
	id := nil.
	self privateDestroy.
	! !

!OpenCLRefCountedObject methodsFor: 'retain/release' stamp: 'jcg 1/15/2010 01:40'!
release
	"Decrement the ref-count.  Once the count hits zero, it is no longer safe to use the object, so clear the ID."
	id ifNil: [^self "either we're already destroyed, or not initialized yet."].
	refCounter release.
	refCounter count = 0 ifTrue: [
		id := nil.
		self privateDestroy
	]! !

!OpenCLRefCountedObject methodsFor: 'retain/release' stamp: 'jcg 1/15/2010 01:40'!
retain
	"Increment the ref-count for this object so that OpenCL will not destroy it."
	id ifNil: [^self "either we're already destroyed, or not initialized yet."].
	refCounter retain.
	! !

!OpenCLRefCountedObject methodsFor: 'initialize' stamp: 'jcg 1/15/2010 01:47'!
id: handle "an OpenCLOpaqueHandle"
	super id: handle.
	refCounter id: handle.! !

!OpenCLRefCountedObject methodsFor: 'initialize' stamp: 'jcg 6/7/2010 01:18'!
initialize
	super initialize.
	refCounter := OpenCLRefCounter new.
	self registerForFinalization.
	! !

!OpenCLRefCountedObject methodsFor: 'initialize' stamp: 'jcg 6/7/2010 01:38'!
invalidateOnStartUp
	"Squeak is starting up, so we're invalid... perform the necessary clean-ups."
	super invalidateOnStartUp.
	self privateDestroy.
! !

!OpenCLRefCountedObject methodsFor: 'initialize' stamp: 'jmv 8/20/2015 16:05'!
registerForFinalization
	"Register our ref-counter as executor, so that it can clean up after we're garbage collected (we don't want to leak any OpenCL resources)."
	OpenCLPrimitiveInterface finalizationRegistry add: self executor: refCounter.
	! !

!OpenCLRefCountedObject methodsFor: 'private' stamp: 'jcg 6/7/2010 01:35'!
privateDestroy
	"Called when the reference count reaches zero.  Subclasses should use this to clean up references to other objects."
	self subclassResponsibility! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jcg 1/14/2010 01:04'!
bufferOfSize: byteSize
	"By default, buffers are read-write."
	^self readWriteBufferOfSize: byteSize! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:49'!
copyReadOnlyBufferFrom: bytes
	"Create an OpenCLBuffer that is only readable.  The size and inital contents are obtained from the 'bytes' argument."
	^self newBuffer: CL_MEM_READ_ONLY copyFrom: bytes! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:47'!
copyReadWriteBufferFrom: bytes
	"Create an OpenCLBuffer that is both readable and writable.  The size and inital contents are obtained from the 'bytes' argument."
	^self newBuffer: CL_MEM_READ_WRITE copyFrom: bytes! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:49'!
copyWriteOnlyBufferFrom: bytes
	"Create an OpenCLBuffer that is only writable.  The size and inital contents are obtained from the 'bytes' argument."
	^self newBuffer: CL_MEM_WRITE_ONLY copyFrom: bytes! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:46'!
readOnlyBufferOfSize: byteSize
	"Create an OpenCLBuffer that is only readable."
	^self newBuffer: CL_MEM_READ_ONLY ofSize: byteSize! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 4/24/2020 17:50:22'!
readOnlyBufferOn: bytes
	"Create an OpenCLBuffer that is only readable. It will work directly on the contents of the 'bytes' argument.
	The 'bytes' argument will be pinned as long as this buffer exists."
	^self newBuffer: CL_MEM_READ_ONLY on: bytes! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:48'!
readWriteBufferOfSize: byteSize
	"Create an OpenCLBuffer that is both readable and writeable."
	^self newBuffer: CL_MEM_READ_WRITE ofSize: byteSize! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 4/24/2020 19:00:00'!
readWriteBufferOn: bytes
	"Create an OpenCLBuffer that is both readable and writable. It will work directly on the contents of the 'bytes' argument.
	The 'bytes' argument will be pinned as long as this buffer exists."
	^self newBuffer: CL_MEM_READ_WRITE on: bytes! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 9/9/2015 11:45'!
writeOnlyBufferOfSize: byteSize
	"Create an OpenCLBuffer that is only writeable."
	^self newBuffer: CL_MEM_WRITE_ONLY ofSize: byteSize! !

!OpenCLContext methodsFor: 'buffer creation' stamp: 'jmv 4/24/2020 19:00:07'!
writeOnlyBufferOn: bytes
	"Create an OpenCLBuffer that is only readable. It will work directly on the contents of the 'bytes' argument.
	The 'bytes' argument will be pinned as long as this buffer exists."
	^self newBuffer: CL_MEM_WRITE_ONLY on: bytes! !

!OpenCLContext methodsFor: 'deprecated in OpenCL 2.0' stamp: 'jmv 10/29/2015 15:40'!
commandQueueOn: anOpenCLDevice
	"Instantate a new OpenCLCommandQueue."
	"Deprecated in OpenCL 2.0:
	The clCreateCommandQueue function was deprecated as of OpenCL 2.0, and replaced with clCreateCommandQueueWithProperties. If you are only targeting devices that support OpenCL 2.0 (some recent Intel and AMD processors at the time of writing), you can safely use this new function.

If you need your code to run on devices that don't yet support OpenCL 2.0, you can continue using the deprecated clCreateCommandQueue function by using the preprocessor macros that the OpenCL headers provide, e.g:

#define CL_USE_DEPRECATED_OPENCL_2_0_APIS
#include <CL/cl.h>
"
	| queue result |
	result := IntegerArray new: 1.
	queue := OpenCLPrimitiveInterface current
				clCreateCommandQueue__cl_context: id 
							cl_device_id: anOpenCLDevice id
							cl_command_queue_properties: 0 "out-of-order exec seems to not work on OS X"
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to create queue'.
	"Wrap the CLCommandQueue with an OpenCLCommandQueue."
	queue := OpenCLCommandQueue wrappingHandle: queue.
	queue context: self device: anOpenCLDevice.
	^queue! !

!OpenCLContext methodsFor: 'deprecated in OpenCL 2.0' stamp: 'jmv 10/29/2015 15:42'!
sampler: useNormalizedCoords addressing: addressingMode filter: filterMode
	"Most general sampler-creation method."
	"Deprecated in OpenCL 2.0
	Replaced with clCreateSamplerWithProperties
	"
	| sampler result |
	result := IntegerArray new: 1.
	sampler := OpenCLPrimitiveInterface current
				clCreateSampler__cl_context: id
							cl_bool: (useNormalizedCoords ifTrue: [CL_TRUE] ifFalse: [CL_FALSE])
							cl_addressing_mode: addressingMode
							cl_filter_mode: filterMode
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to instantiate sampler'.
	sampler := OpenCLSampler wrappingHandle: sampler.
	sampler context: self normalized: useNormalizedCoords addressing: addressingMode filter: filterMode.
	^sampler! !

!OpenCLContext methodsFor: 'accessing' stamp: 'jcg 2/15/2010 12:45'!
devices
	"Answer the list of devices that this context was created with."
	^devices! !

!OpenCLContext methodsFor: 'initialize' stamp: 'jcg 2/15/2010 12:45'!
devices: deviceList "Collection of OpenCLDevices"
	"Answer the list of devices that this context was created with."
	devices := deviceList! !

!OpenCLContext methodsFor: 'private' stamp: 'jcg 2/12/2010 08:57'!
externalStringFor: aString
	| addr |
	addr := ExternalAddress allocate: aString size + 1.
	aString withIndexDo: [:char :i | addr unsignedCharAt: i put: char].
	addr unsignedByteAt: aString size + 1 put: 0.
	^addr! !

!OpenCLContext methodsFor: 'private' stamp: 'jmv 4/24/2020 17:25:30'!
newBuffer: flags copyFrom: bytes
	| mem result |
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateBuffer__cl_context: id
							cl_mem_flags: (flags+CL_MEM_COPY_HOST_PTR)
							size_t: bytes byteSize
							void__ptr: bytes
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to create buffer'.
	mem := OpenCLBuffer wrappingHandle: mem.
	mem context: self byteSize: bytes byteSize.
	^mem
	
	! !

!OpenCLContext methodsFor: 'private' stamp: 'jmv 9/9/2015 12:27'!
newBuffer: flags ofSize: byteSize
	| mem result |
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateBuffer__cl_context: id
							cl_mem_flags: flags
							size_t: byteSize
							void__ptr: nil
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to create buffer'.
	mem := OpenCLBuffer wrappingHandle: mem.
	mem context: self byteSize: byteSize.
	^mem
	
	! !

!OpenCLContext methodsFor: 'private' stamp: 'jmv 4/24/2020 17:42:02'!
newBuffer: flags on: bytes
	"Using a Smalltalk object from OpenCL requires pinning it.
	You might unpit it when done."
	| mem result mustPinUnpin |
	mustPinUnpin _ bytes isPinned not.
	mustPinUnpin ifTrue: [ bytes pin ].
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateBuffer__cl_context: id
							cl_mem_flags: (flags+CL_MEM_USE_HOST_PTR)
							size_t: bytes byteSize
							void__ptr: bytes
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to create buffer'.
	mem := OpenCLBuffer wrappingHandle: mem.
	mem context: self byteSize: bytes byteSize.
	mustPinUnpin ifTrue: [
		mem smalltalkObject: bytes].
	^mem! !

!OpenCLContext methodsFor: 'private' stamp: 'jcg 1/14/2010 00:17'!
privateDestroy
	"Nothing needs to be done."! !

!OpenCLContext methodsFor: 'deprecated in OpenCL 1.2' stamp: 'jmv 10/29/2015 16:39'!
newImage2D: flags format: imageFormat width: width height: height rowPitch: rowPitch hostPointer: hostPtr
	"Most general 2D image-creation method.  Basis for more specific, convenient methods."
	"Deprecated in OpenCL1.2:
	clCreateImage2D and clCreateImage3D have been merged into clCreateImage"
	| mem result |
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateImage2D__cl_context: id
							cl_mem_flags: flags
							const__cl_image_format__ptr: imageFormat
							size_t: width
							size_t: height
							size_t: rowPitch
							void__ptr: hostPtr
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to instantiate 2D image'.
	mem := OpenCLImage2D wrappingHandle: mem.
	mem context: self format: imageFormat width: width height: height rowPitch: rowPitch.
	^mem! !

!OpenCLContext methodsFor: 'deprecated in OpenCL 1.2' stamp: 'jmv 10/29/2015 16:39'!
newImage3D: flags format: imageFormat width: width height: height depth: depth rowPitch: rowPitch slicePitch: slicePitch hostPointer: hostPtr
	"Most general 3D image-creation method.  Basis for more specific, convenient methods."
	"Deprecated in OpenCL1.2:
	clCreateImage2D and clCreateImage3D have been merged into clCreateImage"
	| mem result |
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateImage3D__cl_context: id
							cl_mem_flags: flags
							const__cl_image_format__ptr: imageFormat
							size_t: width
							size_t: height
							size_t: depth
							size_t: rowPitch
							size_t: slicePitch
							void__ptr: hostPtr
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to instantiate 3D image'.
	mem := OpenCLImage3D wrappingHandle: mem.
	mem 
		context: self 
		format: imageFormat 
		width: width 
		height: height 
		depth: depth 
		rowPitch: rowPitch 
		slicePitch: slicePitch.
	^mem! !

!OpenCLContext methodsFor: 'image creation' stamp: 'jmv 11/6/2017 13:17:50'!
newImage: flags format: imageFormat description: imageDescription hostPointer: hostPtr
	"Most general image-creation method.  Basis for more specific, convenient methods."
	| mem result |
	result := IntegerArray new: 1.
	mem := OpenCLPrimitiveInterface current
				clCreateImage__cl_context: id
							cl_mem_flags: flags
							const__cl_image_format__ptr: imageFormat
							const__cl_image_desc__ptr: imageDescription
							void__ptr: hostPtr asOpenCLArg
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to instantiate image. image_desc: ', imageDescription printString.

	imageDescription image_type = CL_MEM_OBJECT_IMAGE2D ifTrue: [
		mem := OpenCLImage2D wrappingHandle: mem.
		mem context: self format: imageFormat width: imageDescription image_width height: imageDescription image_height rowPitch: imageDescription image_row_pitch.
		^mem ].

	self error: 'Support this case here!!'! !

!OpenCLContext methodsFor: 'image creation' stamp: 'jmv 12/20/2016 22:58:17'!
supportedImageFormats: flags type: imageType "CL_MEM_OBJECT_IMAGE2D or 3D"
	| result maxResults numResults bytes byteSize |
	maxResults := 200.  "The maximum number of results... bump this up if necessary."
	numResults := IntegerArray new: 1.
	byteSize := CLImageFormatStruct byteSize.
	bytes := ByteArray new: (maxResults * byteSize).
	result := OpenCLPrimitiveInterface current
				clGetSupportedImageFormats__cl_context: id
							cl_mem_flags: flags
							cl_mem_object_type: imageType
							cl_uint: maxResults
							cl_image_format__ptr: (CLImageFormatStruct new setHandle: bytes)
							cl_uint__ptr: numResults.
	OpenCLError check: result text: 'failed to obtain supported image formats'.
	
	"Construct a list of CLImageFormatStructs to return."
	numResults := numResults first.
	^(1 to: numResults) collect: [:i |
		| struct |
		struct := CLImageFormatStruct new.
		struct image_channel_order: (bytes unsignedLongAt: (i-1*byteSize+1)).
		struct image_channel_data_type: (bytes unsignedLongAt: (i-1*byteSize+1+(byteSize/2))).
	]
	

	! !

!OpenCLContext methodsFor: 'sampler creation' stamp: 'jmv 9/9/2015 11:49'!
normalizedLinearSampler: addressingMode
	^self sampler: true addressing: addressingMode filter: CL_FILTER_LINEAR! !

!OpenCLContext methodsFor: 'sampler creation' stamp: 'jmv 9/9/2015 11:46'!
normalizedNearestSampler: addressingMode
	^self sampler: true addressing: addressingMode filter: CL_FILTER_NEAREST! !

!OpenCLContext methodsFor: 'sampler creation' stamp: 'jmv 9/9/2015 11:45'!
unnormalizedLinearSampler: addressingMode
	^self sampler: false addressing: addressingMode filter: CL_FILTER_LINEAR! !

!OpenCLContext methodsFor: 'sampler creation' stamp: 'jmv 9/9/2015 11:48'!
unnormalizedNearestSampler: addressingMode
	^self sampler: false addressing: addressingMode filter: CL_FILTER_NEAREST! !

!OpenCLContext methodsFor: 'program creation' stamp: 'jmv 12/22/2016 15:29:15'!
programWithSource: stringOrListOfStrings
	| strings pointers program result  |
	
	"Easier if we uniformly deal with a list of strings."
	stringOrListOfStrings isString
		ifTrue: [strings := {stringOrListOfStrings}]
		ifFalse: [strings := stringOrListOfStrings].
		
	"Transform the strings into null-terminated strings on the C heap."
	strings := strings collect: [:s | self externalStringFor: s].
	
	"Put all string pointers into a single list."
	pointers := WriteStream on: (ByteArray new: strings size * Smalltalk wordSize).
	strings do: [:s | pointers nextPutAll: s].
	pointers := pointers contents.
	
	"Make the OpenCL API call to create the program."
	result := IntegerArray new: 1.
	[program := OpenCLPrimitiveInterface current
					clCreateProgramWithSource__cl_context: id
								cl_uint: strings size
								const__char__ptr__ptr: pointers
								const__size_t__ptr: nil "null-terminated, so no lengths necessary"
								cl_int__ptr: result.
	] ensure: [strings do: [:s | s free]].				
	result := result first.
	OpenCLError check: result text: 'failed to instantiate program'.
	program := OpenCLProgram wrappingHandle: program.
	program context: self sourceCode: stringOrListOfStrings.
	^program
	! !

!OpenCLContext methodsFor: 'report' stamp: 'jmv 9/16/2015 17:20'!
reportImageFormatsOn: aStream
	"
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].
	"
	aStream nextPutAll: 'Image Formats for devices: { ['.
	self devices do: [ :device | aStream nextPutAll: device name ] separatedBy: [ aStream nextPutAll: '], [' ].
	aStream nextPutAll: '] }'; newLine.
	aStream nextPutAll: '--- Image formats for CL_MEM_READ_WRITE ---'; newLine.
	(self supportedImageFormats: 1 "CL_MEM_READ_WRITE" type: 16r10F1"CL_MEM_OBJECT_IMAGE2D") do: [ :each |
		aStream nextPutAll: each printString; newLine ].
	aStream nextPutAll: '--- Image formats for CL_MEM_WRITE_ONLY ---'; newLine.
	(self supportedImageFormats: 2 "CL_MEM_WRITE_ONLY" type: 16r10F1"CL_MEM_OBJECT_IMAGE2D") do: [ :each | 
		aStream nextPutAll: each printString; newLine ].
	aStream nextPutAll: '--- Image formats for CL_MEM_READ_ONLY ---'; newLine.
	(self supportedImageFormats: 4 "CL_MEM_READ_ONLY" type: 16r10F1"CL_MEM_OBJECT_IMAGE2D") do: [ :each |
		aStream nextPutAll: each printString; newLine ]! !

!OpenCLContext class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 12:28'!
newForDevices: deviceList "of OpenCLDevices" 
	| context allHandles result |
	
	allHandles := (deviceList collect: [:dev | dev id getHandle]) concatenation asByteArray.
	result := IntegerArray new: 1.
	context := OpenCLPrimitiveInterface current
				clCreateContext__const__cl_context_properties__ptr: nil
							cl_uint: deviceList size
							const__cl_device_id__ptr: (CLDeviceID fromHandle: allHandles)
							void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: nil "error callback"
							void__ptr: nil "user-data for error callback"
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to create context'.
	context := self wrappingHandle: context.
	context devices: deviceList.
	^context
				! !

!OpenCLContextObject methodsFor: 'accessing' stamp: 'jcg 2/12/2010 09:22'!
context
	"Answer the OpenCLContext that this object is associated with."
	^context! !

!OpenCLContextObject methodsFor: 'private' stamp: 'jcg 2/12/2010 09:21'!
privateDestroy
	"Subclasses may extend this method, but must ensure that 'context' is set to nil."
	context := nil.! !

!OpenCLBuffer methodsFor: 'accessing' stamp: 'jcg 1/14/2010 01:31'!
byteSize
	"Answer the size of memory allocated for this buffer, in bytes."
	^byteSize! !

!OpenCLBuffer methodsFor: 'accessing' stamp: 'jmv 4/24/2020 17:41:45'!
smalltalkObject: bytes
	smalltalkObject _ bytes! !

!OpenCLBuffer methodsFor: 'initialize' stamp: 'jcg 1/14/2010 01:21'!
context: anOpenCLContext byteSize: sz
	"Set the context that this buffer was created for, and it's size in bytes."
	context := anOpenCLContext.
	byteSize := sz.
! !

!OpenCLBuffer methodsFor: 'private' stamp: 'jmv 4/24/2020 17:45:12'!
privateDestroy
	"If we were created on a Smalltalk object, it was pinned if needed.
	If so, unpin it, and don't reference it anymore so it can be collected.
	
	Note: Creating two different OpenCLBuffers on the same byte object is not supported wither by OpenCL or by this code"
	super privateDestroy.
	smalltalkObject notNil ifTrue: [
		smalltalkObject unpin.
		smalltalkObject  _ nil ].! !

!OpenCLCommandQueue methodsFor: 'deprecated in OpenCL 1.2' stamp: 'jmv 10/29/2015 16:39'!
barrier
	"Ensures that all currently-queued commands will finish before any subsequently-queued commands are executed..  This should be equivalent to 'queue waitForEvents: queue marker' (of course, doing it with the C API would be somewhat less convenient)."
	"Deprecated in OpenCL1.2:
	clEnqueueMarker, clEnqueueBarrier and clEnqueueWaitForEvents have been merged into clEnqueueMarkerWithWaitList and clEnqueueBarrierWithWaitList. The barrier and marker functionality are still the same, but if a non-NULL waiting-list is given it will also continue if all the events have occurred. This was tricky to program before. A new option is that you can fire an event when all previous events have occurred.
	"
	| result |
	result := OpenCLPrimitiveInterface current clEnqueueBarrier__cl_command_queue: id.
	OpenCLError check: result text: 'failed to enqueue barrier'.! !

!OpenCLCommandQueue methodsFor: 'deprecated in OpenCL 1.2' stamp: 'jmv 10/29/2015 16:39'!
marker
	"Obtain a marker event (an OpenCLEvent) that won't complete until all previously-queued events have completed."
	"Deprecated in OpenCL1.2:
	clEnqueueMarker, clEnqueueBarrier and clEnqueueWaitForEvents have been merged into clEnqueueMarkerWithWaitList and clEnqueueBarrierWithWaitList. The barrier and marker functionality are still the same, but if a non-NULL waiting-list is given it will also continue if all the events have occurred. This was tricky to program before. A new option is that you can fire an event when all previous events have occurred.
	"
	| result evt |
	result := OpenCLPrimitiveInterface current
		clEnqueueMarker__cl_command_queue: id 
					cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to obtain marker event'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'deprecated in OpenCL 1.2' stamp: 'jmv 10/29/2015 16:39'!
waitForEvents: events "single OpenCLEvent, or list of them"
	"Enqueue a wait for the specified events, so that any future commands will wait until all the events have completed."
	"Deprecated in OpenCL1.2:
	clEnqueueMarker, clEnqueueBarrier and clEnqueueWaitForEvents have been merged into clEnqueueMarkerWithWaitList and clEnqueueBarrierWithWaitList. The barrier and marker functionality are still the same, but if a non-NULL waiting-list is given it will also continue if all the events have occurred. This was tricky to program before. A new option is that you can fire an event when all previous events have occurred.
	"
	| waitList result |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current 
				clEnqueueWaitForEvents__cl_command_queue: id 
							cl_uint: waitList first 
							const__cl_event__ptr: waitList second.
	OpenCLError check: result text: 'failed to enqueue wait for events'! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jmv 10/29/2015 10:41'!
beInOrder
	"The spec warns that this can be slow, since OpenCL will block until all currently-queued commands have finished."
	| result |
	isInOrder ifTrue: [^self].
	"Warning: clSetCommandQueueProperty() was deprecated in OpenCL 1.1 and is no longer in headers in 1.2 and 2.0.
	I believe callers should be converted to use clCreateCommandQueueWithProperties()"
	result := OpenCLPrimitiveInterface current
				clSetCommandQueueProperty__cl_command_queue: id
							cl_command_queue_properties: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
							cl_bool: CL_TRUE
							cl_command_queue_properties__ptr: nil.
	OpenCLError check: result.
	isInOrder := true. "only if we succeed"! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jmv 10/29/2015 10:41'!
beOutOfOrder
	"The spec warns that this can be slow, since OpenCL will block until all currently-queued commands have finished."
	| result |
	isInOrder ifFalse: [^self].
	"Warning: clSetCommandQueueProperty() was deprecated in OpenCL 1.1 and is no longer in headers in 1.2 and 2.0.
	I believe callers should be converted to use clCreateCommandQueueWithProperties()"
	result := OpenCLPrimitiveInterface current
				clSetCommandQueueProperty__cl_command_queue: id
							cl_command_queue_properties: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
							cl_bool: CL_FALSE
							cl_command_queue_properties__ptr: nil.
	OpenCLError check: result.
	isInOrder := false. "only if we succeed"! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jcg 1/14/2010 00:10'!
isInOrder
	^isInOrder! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jcg 1/14/2010 00:10'!
isProfiling
	^isProfiling! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jmv 10/29/2015 10:41'!
startProfiling
	"Enable profiling on this queue... once an event has finished processing, we can query how long it took to process."
	| result |
	isProfiling ifTrue: [^self].
	"Warning: clSetCommandQueueProperty() was deprecated in OpenCL 1.1 and is no longer in headers in 1.2 and 2.0.
	I believe callers should be converted to use clCreateCommandQueueWithProperties()"
	result := OpenCLPrimitiveInterface current
				clSetCommandQueueProperty__cl_command_queue: id
							cl_command_queue_properties: CL_QUEUE_PROFILING_ENABLE
							cl_bool: CL_TRUE
							cl_command_queue_properties__ptr: nil.
	OpenCLError check: result.
	isProfiling := true. "only if we succeed"! !

!OpenCLCommandQueue methodsFor: 'properties' stamp: 'jmv 10/29/2015 10:41'!
stopProfiling
	"Stop profiling on this queue."
	| result |
	isProfiling ifFalse: [^self].
	"Warning: clSetCommandQueueProperty() was deprecated in OpenCL 1.1 and is no longer in headers in 1.2 and 2.0.
	I believe callers should be converted to use clCreateCommandQueueWithProperties()"
	result := OpenCLPrimitiveInterface current
				clSetCommandQueueProperty__cl_command_queue: id
							cl_command_queue_properties: CL_QUEUE_PROFILING_ENABLE
							cl_bool: CL_FALSE
							cl_command_queue_properties__ptr: nil.
	OpenCLError check: result.
	isProfiling := false. "only if we succeed"! !

!OpenCLCommandQueue methodsFor: 'initialize' stamp: 'jcg 1/14/2010 01:03'!
context: anOpenCLContext device: anOpenCLDevice
	"Set the context and device that this queue was created for."
	context := anOpenCLContext.
	device := anOpenCLDevice.
! !

!OpenCLCommandQueue methodsFor: 'initialize' stamp: 'jcg 1/13/2010 23:40'!
initialize
	super initialize.
	isInOrder := false.  "See #newQueueOn:"
	isProfiling := false. "ditto"! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 9/9/2015 12:48'!
copyBuffer: srcBuf "an OpenCLBuffer" at: srcOffset into: destBuf "an OpenCLBuffer" at: destOffset size: sz waitOn: events "nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueCopyBuffer__cl_command_queue: id
							cl_mem: srcBuf id
							cl_mem: destBuf id
							size_t: srcOffset
							size_t: destOffset
							size_t: sz
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue copy-buffer'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 12/26/2016 09:31:48'!
copyBuffer: srcBuffer offset: srcOffset intoImage: destImage origin: destOrigin region: destRegion waitOn: events
	"Arguments:
		srcBuffer	-		An OpenCLBuffer
		srcOffset	-		integer
		destImage	-		An OpenCLImage2D/3D
		destOrigin	-		3-element Array or IntegerArray
		destRegion	-		ditto
		events		-		nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueCopyBufferToImage__cl_command_queue: id
							cl_mem: srcBuffer id
							cl_mem: destImage id
							size_t: srcOffset
							const__size_t__ptr: destOrigin asMachineWordArray
							const__size_t__ptr: destRegion asMachineWordArray
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue copy-buffer-to-image'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 12/26/2016 09:31:55'!
copyImage: srcImage origin: srcOrigin into: destImage origin: destOrigin region: region waitOn: events
	"Arguments:
		srcImage	-		An OpenCLImage2D/3D
		srcOrigin	-		3-element Array or IntegerArray
		destImage	-		An OpenCLImage2D/3D
		destOrigin	-		3-element Array or IntegerArray
		region		-		ditto
		events		-		nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueCopyImage__cl_command_queue: id
							cl_mem: srcImage id
							cl_mem: destImage id
							const__size_t__ptr: srcOrigin asMachineWordArray
							const__size_t__ptr: destOrigin asMachineWordArray
							const__size_t__ptr: region asMachineWordArray
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue copy-image'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 12/26/2016 09:32:02'!
copyImage: srcImage origin: srcOrigin region: srcRegion intoBuffer: destBuffer offset: destOffset waitOn: events
	"Arguments:
		srcImage	-		An OpenCLImage2D/3D
		srcOrigin	-		3-element Array or IntegerArray
		srcRegion	-		ditto
		destBuffer	-		An OpenCLBuffer
		destOffset	-		integer
		events		-		nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueCopyImageToBuffer__cl_command_queue: id
							cl_mem: srcImage id
							cl_mem: destBuffer id
							const__size_t__ptr: srcOrigin asMachineWordArray
							const__size_t__ptr: srcRegion asMachineWordArray
							size_t: destOffset asMachineWordArray
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue copy-image-to-buffer'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 9/9/2015 12:44'!
readBuffer: buf "an OpenCLBuffer" block: shouldBlock "boolean" offset: offset size: sz pointer: ptr "a ByteArray or ExternalAddress" waitOn: events "nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueReadBuffer__cl_command_queue: id
							cl_mem: buf id
							cl_bool: shouldBlock asOpenCLBoolean
							size_t: offset
							size_t: sz
							void__ptr: ptr
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue read-buffer'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jcg 2/15/2010 23:45'!
readBuffer: buf into: bytes "a ByteArray" waitOn: events
	"Convenient blocking read into a ByteArray (or similar type, like IntegerArray or FloatArray)."
	^self readBuffer: buf block: true offset: 0 size: bytes byteSize pointer: bytes waitOn: events! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 12/26/2016 09:30:36'!
readImage: image block: shouldBlock origin: origin region: region rowPitch: rowPitch slicePitch: slicePitch pointer: ptr waitOn: events
	"Arguments:
		image		-		An OpenCLImage2D/3D
		shouldBlock	-		boolean
		origin		-		3-element Array or IntegerArray
		region		-		ditto
		rowPitch	-		integer
		slicePitch	-		integer
		ptr			-		a ByteArray or ExternalAddress
		events		-		nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueReadImage__cl_command_queue: id
							cl_mem: image id
							cl_bool: shouldBlock asOpenCLBoolean
							const__size_t__ptr: origin asMachineWordArray
							const__size_t__ptr: region asMachineWordArray
							size_t: rowPitch
							size_t: slicePitch
							void__ptr: ptr asOpenCLArg
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue read-image'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 9/9/2015 12:47'!
writeBuffer: buf "an OpenCLBuffer" block: shouldBlock "boolean" offset: offset size: sz pointer: ptr "a ByteArray or ExternalAddress" waitOn: events "nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	evt := CLEvent new.
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueWriteBuffer__cl_command_queue: id
							cl_mem: buf id
							cl_bool: shouldBlock asOpenCLBoolean
							size_t: offset
							size_t: sz
							const__void__ptr: ptr
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue write-buffer'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jcg 2/15/2010 23:45'!
writeBuffer: buf from: bytes "a ByteArray" waitOn: events
	"Convenient blocking write from a ByteArray (or similar type, like IntegerArray or FloatArray)."
	^self writeBuffer: buf block: true offset: 0 size: bytes byteSize pointer: bytes waitOn: events! !

!OpenCLCommandQueue methodsFor: 'mem read/write/copy' stamp: 'jmv 12/26/2016 09:32:28'!
writeImage: image block: shouldBlock origin: origin region: region rowPitch: rowPitch slicePitch: slicePitch pointer: ptr waitOn: events
	"Arguments:
		image		-		An OpenCLImage2D/3D
		shouldBlock	-		boolean
		origin		-		3-element Array or IntegerArray
		region		-		ditto
		rowPitch	-		integer
		slicePitch	-		integer
		ptr			-		a ByteArray or ExternalAddress
		events		-		nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueWriteImage__cl_command_queue: id
							cl_mem: image id
							cl_bool: shouldBlock asOpenCLBoolean
							const__size_t__ptr: origin asMachineWordArray
							const__size_t__ptr: region asMachineWordArray
							size_t: rowPitch
							size_t: slicePitch
							const__void__ptr: ptr asOpenCLArg
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue write-image'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'accessing' stamp: 'jcg 1/14/2010 00:12'!
device
	"Answer the OpenCLDevice that this queue belongs to."
	^device! !

!OpenCLCommandQueue methodsFor: 'commands - synchronization' stamp: 'jmv 9/9/2015 12:44'!
finish
	"Blocks until all previously-queued commands in the queue have completed."
	| result |
	result := OpenCLPrimitiveInterface current clFinish__cl_command_queue: id.
	OpenCLError check: result text: 'failed to finish events in the command-queue'.
! !

!OpenCLCommandQueue methodsFor: 'command sync' stamp: 'jmv 9/9/2015 12:47'!
flush
	"Issues all previously-queued commands to the device associated with the queue (i.e. so that they won't potentially be buffered forever in the host-API."
	| result |
	result := OpenCLPrimitiveInterface current clFlush__cl_command_queue: id.
	OpenCLError check: result text: 'failed to flush command-queue'.
! !

!OpenCLCommandQueue methodsFor: 'mem mapping' stamp: 'jmv 9/9/2015 12:33'!
mapBuffer: buf block: shouldBlock flags: flags offset: offset size: sz waitOn: events
	"This is the most general buffer-mapping method; it corresponds directly to the OpenCL API call.  In practice, more convenient methods will allow omitting many arguments."
	"Arguments:
		buf			-	An OpenCLBuffer
		shouldBlock	-	boolean
		flags		-	CL_MAP_READ and/or CL_MAP_WRITE
		offset		-	integer offset
		size		-	size of area to map
		events		-	nil, single OpenCLEvent, or list of them"
	| ptr evt waitList result |
	waitList := OpenCLEvent generateWaitList: events.
	result := IntegerArray new: 1.
	ptr := OpenCLPrimitiveInterface current
			clEnqueueMapBuffer__cl_command_queue: id
						cl_mem: buf id
						cl_bool: shouldBlock
						cl_map_flags: flags
						size_t: offset
						size_t: sz
						cl_uint: waitList first "number of events to wait on"
						const__cl_event__ptr: waitList second "concatenated event handles"
						cl_event__ptr: (evt := CLEvent new)
						cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to enqueue map-buffer'.
	(evt := OpenCLMapEvent wrappingHandle: evt)
		commandQueue: self;
		pointer: ptr.
	^evt
	! !

!OpenCLCommandQueue methodsFor: 'mem mapping' stamp: 'jmv 5/15/2020 15:06:42'!
mapBuffer: buf waitOn: events

	^self mapBuffer: buf block: true flags: CL_MAP_WRITE + CL_MAP_READ offset: 0 size: buf byteSize waitOn: events! !

!OpenCLCommandQueue methodsFor: 'mem mapping' stamp: 'jmv 12/26/2016 09:32:10'!
mapImage: image block: shouldBlock flags: flags origin: origin region: region waitOn: events
	"This is the most general image-mapping method; it corresponds directly to the OpenCL API call.  In practice, more convenient methods will allow omitting many arguments."
	"Arguments:
		image			-	An OpenCLBuffer
		shouldBlock	-	boolean
		flags			-	CL_MAP_READ and/or CL_MAP_WRITE, CL_MAP_WRITE_INVALIDATE_REGION
		origin			-		3-element Array or IntegerArray
		region			-		ditto
		events		-	nil, single OpenCLEvent, or list of them"
	| ptr evt waitList result rowPitch slicePitch|
	waitList := OpenCLEvent generateWaitList: events.
	result := IntegerArray new: 1.
	rowPitch := IntegerArray new: 1.
	slicePitch := IntegerArray new: 1.
	ptr := OpenCLPrimitiveInterface current
			clEnqueueMapImage__cl_command_queue: id
						cl_mem: image id
						cl_bool: shouldBlock
						cl_map_flags: flags
						const__size_t__ptr: origin asMachineWordArray
						const__size_t__ptr: region asMachineWordArray
						size_t__ptr: rowPitch
						size_t__ptr: slicePitch
						cl_uint: waitList first "number of events to wait on"
						const__cl_event__ptr: waitList second "concatenated event handles"
						cl_event__ptr: (evt := CLEvent new)
						cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed to enqueue map-image'.
	(evt := OpenCLMapEvent wrappingHandle: evt)
		commandQueue: self;
		pointer: ptr;
		rowPitch: rowPitch first;
		slicePitch: slicePitch first.
	^evt
	! !

!OpenCLCommandQueue methodsFor: 'mem mapping' stamp: 'jmv 9/9/2015 12:31'!
unmap: memObject "image or buffer" pointer: ptr waitOn: events "nil, single OpenCLEvent, or list of them"
	| evt waitList result |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueUnmapMemObject__cl_command_queue: id
							cl_mem: memObject id
							void__ptr: ptr
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).
	OpenCLError check: result text: 'failed to enqueue unmap-mem-object'.
	^self wrapEventHandle: evt
	
	! !

!OpenCLCommandQueue methodsFor: 'kernels and tasks' stamp: 'jmv 12/26/2016 09:32:22'!
runKernel: kernel workDim: workDim globalOffset: globalWorkOffset globalSize: globalWorkSize localSize: localWorkSize waitOn: events
	"Arguments
		kernel					-	An OpenCLKernel
		workDim				-	# of dimensions used to specify local/global work sizes/offsets
										(the sizes of the other arguments must match)
		globalWorkOffset		-	array of integers
		globalWorkSize		-	array of integers
		localWorkSize			-	array of integers
		events					-	nil, single OpenCLEvent, or list of them"
	| result evt waitList |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current
				clEnqueueNDRangeKernel__cl_command_queue: id
							cl_kernel: kernel id
							cl_uint: workDim
							const__size_t__ptr: globalWorkOffset asMachineWordArray
							const__size_t__ptr: globalWorkSize asMachineWordArray
							const__size_t__ptr: localWorkSize asMachineWordArray
							cl_uint: waitList first "number of events to wait on"
							const__cl_event__ptr: waitList second "concatenated event handles"
							cl_event__ptr: (evt := CLEvent new).	
	OpenCLError check: result text: 'failed to enqueue kernel for execution'.
	^self wrapEventHandle: evt! !

!OpenCLCommandQueue methodsFor: 'private' stamp: 'jcg 1/15/2010 00:30'!
wrapEventHandle: aCLEvent
	"Answer an event corresponding to an enqueued command."
	^(OpenCLEvent wrappingHandle: aCLEvent) commandQueue: self! !

!OpenCLImage2D methodsFor: 'accessing' stamp: 'jcg 4/28/2010 00:07'!
extent
	^width@height! !

!OpenCLImage2D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
height
	^height! !

!OpenCLImage2D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
rowPitch
	^rowPitch! !

!OpenCLImage2D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
width
	^width! !

!OpenCLImage2D methodsFor: 'initialize' stamp: 'jcg 1/29/2010 02:08'!
context: anOpenCLContext format: fmt "a CLImageFormatStruct" width: w height: h rowPitch: rp 
	"Set the context that this buffer was created for, as well as its dimensions and format." 
	context := anOpenCLContext.
	format := fmt.
	width := w.
	height := h.
	rowPitch := rp.! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
depth
	^depth! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
height
	^height! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
rowPitch
	^rowPitch! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:19'!
slicePitch
	^slicePitch! !

!OpenCLImage3D methodsFor: 'accessing' stamp: 'jcg 1/29/2010 02:18'!
width
	^width! !

!OpenCLImage3D methodsFor: 'initialize' stamp: 'jmv 5/4/2016 12:41'!
context: anOpenCLContext format: fmt "a CLImageFormatStruct" width: w height: h depth: d rowPitch: rp slicePitch: sp
	"Set the context that this buffer was created for, as well as its dimensions and format." 
	context := anOpenCLContext.
	format := fmt.
	width := w.
	height := h.
	depth := d.
	rowPitch := rp.
	slicePitch := sp! !

!OpenCLProgram methodsFor: 'building' stamp: 'jcg 2/15/2010 12:53'!
build
	"Build the program with the default settings, for all devices associated with the program's context."
	self buildDevices: context devices options: ''
! !

!OpenCLProgram methodsFor: 'building' stamp: 'jcg 2/15/2010 12:50'!
buildDevices: deviceOrDeviceList
	"Build the program for the specified devices with the default settings."
	self buildDevices: deviceOrDeviceList options: ''
! !

!OpenCLProgram methodsFor: 'building' stamp: 'jmv 12/23/2016 15:29:20'!
buildDevices: deviceOrDeviceList options: optionsString
	| devices bytes devicesArg result handleSize |
	
	"Ensure that we have a list of at least one device."
	devices := deviceOrDeviceList.
	devices isCollection ifFalse: [devices := {devices}]..
	devices size == 0 ifTrue: [self error: 'no devices specified'].
	
	"Create the list of devices that we will pass to the FFI call."
	handleSize := CLDeviceID byteSize.
	bytes := ByteArray new: (devices size * handleSize).
	devices withIndexDo: [:dev :i | bytes replaceFrom: (i-1*handleSize+1) to: (i*handleSize) with: dev id getHandle].
	devicesArg := CLDeviceID new setHandle: bytes.
	
	"FFI call to build the program."
	result := OpenCLPrimitiveInterface current
				clBuildProgram__cl_program: id
							cl_uint: devices size
							const__cl_device_id__ptr: devicesArg
							const__char__ptr: optionsString
							void__ptrpfn_notifycl_program: nil "no callback yet"
							void__ptr: nil. "no callback yet"
	[OpenCLError check: result text: 'failed to build program']
		on: Error 
		do: [:err |
			false not ifTrue: [ "enable / disable debugging"
				(devices collect: [:dev | self buildLog: dev]) 
					inspectWithLabel: 'OpenCL Compilation Error'
			].
			err pass. 
		]
! !

!OpenCLProgram methodsFor: 'building' stamp: 'jmv 9/9/2015 12:51'!
buildLog: device
	"Obtain the build-log of the last build attempt for the specfied device."
	| apiResult buildResult |
	buildResult := String new: 1000.
	apiResult := OpenCLPrimitiveInterface current
					clGetProgramBuildInfo__cl_program: id
								cl_device_id: device id
								cl_program_build_info: CL_PROGRAM_BUILD_LOG
								size_t: buildResult size
								void__ptr: buildResult
								size_t__ptr: nil.
	OpenCLError check: apiResult text: 'failed to obtain program build-log'.
	^buildResult! !

!OpenCLProgram methodsFor: 'building' stamp: 'jcg 2/15/2010 12:52'!
buildOptions: optionsString
	"Build the program with the specified options, for all devices associated with the program's context."
	self buildDevices: context devices options: optionsString
! !

!OpenCLProgram methodsFor: 'building' stamp: 'jmv 12/26/2016 09:28:10'!
buildStatus: device
	"Obtain the status of the last build attempt for the specfied device.  Answers one of CLBuildNone, CLBuildError, CLBuildSuccess, or CLBuildInProgress."
	| apiResult buildResult |
	buildResult := IntegerArray new: 1.
	apiResult := OpenCLPrimitiveInterface current
					clGetProgramBuildInfo__cl_program: id
								cl_device_id: device id
								cl_program_build_info: CL_PROGRAM_BUILD_STATUS
								size_t: Smalltalk wordSize
								void__ptr: buildResult
								size_t__ptr: nil.
	OpenCLError check: apiResult text: 'failed to obtain program build-info'.
	^buildResult first! !

!OpenCLProgram methodsFor: 'initialize' stamp: 'jcg 2/12/2010 09:17'!
context: anOpenCLContext sourceCode: stringOrListOfStrings
	"Set the context that this buffer was created for, as well as its dimensions and format." 
	context := anOpenCLContext.
	sourceCode := stringOrListOfStrings.
! !

!OpenCLProgram methodsFor: 'kernel creation' stamp: 'jmv 9/9/2015 12:50'!
createKernel: kernelName
	"Create an OpenCLKernel for this program.  The program must already have been successfully built."
	| result kernel |
	result := IntegerArray new: 1.
	kernel := OpenCLPrimitiveInterface current
				clCreateKernel__cl_program: id
							const__char__ptr: kernelName
							cl_int__ptr: result.
	result := result first.
	OpenCLError check: result text: 'failed create kernel named ', kernelName.
	kernel := OpenCLKernel wrappingHandle: kernel.
	kernel program: self.
	^kernel! !

!OpenCLSampler methodsFor: 'accessing' stamp: 'jcg 2/11/2010 00:59'!
addressingMode
	^addressingMode! !

!OpenCLSampler methodsFor: 'accessing' stamp: 'jcg 2/11/2010 00:59'!
filterMode
	^filterMode! !

!OpenCLSampler methodsFor: 'accessing' stamp: 'jcg 2/11/2010 00:59'!
useNormalizedCoords
	^useNormalizedCoords! !

!OpenCLSampler methodsFor: 'initialize' stamp: 'jcg 2/11/2010 01:00'!
context: anOpenCLContext normalized: normalized "boolean" addressing: aMode "cl_addressing_mode enum" filter: fMode "cl_filter_mode enum"
	"Set the context and paramters that this sampler was created with." 
	context := anOpenCLContext.
	useNormalizedCoords := normalized.
	addressingMode := aMode.
	filterMode := fMode.! !

!OpenCLEvent methodsFor: 'accessing' stamp: 'jcg 1/14/2010 02:00'!
commandQueue
	"Answer the command-queue that the command corresponding to this event was enqueued upon."
		^commandQueue! !

!OpenCLEvent methodsFor: 'accessing' stamp: 'jmv 9/9/2015 11:46'!
commandType
	"Answer an integer representing the cl_command_type of the event."
	| bytes |
	bytes := ByteArray new: 4.
	self getEventInfo: CL_EVENT_COMMAND_TYPE into: bytes.
	^bytes unsignedLongAt: 1 bigEndian: Smalltalk isBigEndian! !

!OpenCLEvent methodsFor: 'accessing' stamp: 'jmv 2/24/2016 15:19'!
executionStatus
	"Answer an integer representing the execution status of the event."
	| bytes |
	bytes := ByteArray new: 4.
	self getEventInfo: CL_EVENT_COMMAND_EXECUTION_STATUS into: bytes.
	^bytes unsignedLongAt: 1! !

!OpenCLEvent methodsFor: 'initialize' stamp: 'jcg 1/14/2010 02:01'!
commandQueue: anOpenCLCommandQueue
	"Set the command-queue that the command corresponding to this event was enqueued upon."
	commandQueue := anOpenCLCommandQueue.! !

!OpenCLEvent methodsFor: 'private' stamp: 'jmv 9/9/2015 12:54'!
getEventInfo:  param into: bytes
	| result |
	result := OpenCLPrimitiveInterface current
				clGetEventInfo__cl_event: id
							cl_event_info: param
							size_t: bytes byteSize
							void__ptr: bytes
							size_t__ptr: nil.
	OpenCLError check: result text: 'failed to get event-info'.
! !

!OpenCLEvent methodsFor: 'private' stamp: 'jcg 1/14/2010 02:00'!
privateDestroy
	commandQueue := nil.
	! !

!OpenCLEvent methodsFor: 'testing' stamp: 'jmv 9/9/2015 11:47'!
isComplete
	"Is the event currently in the 'complete' state?"
	^self executionStatus == CL_COMPLETE! !

!OpenCLEvent methodsFor: 'testing' stamp: 'jmv 9/9/2015 11:48'!
isQueued
	"Is the event currently in the 'queued' state?"
	^self executionStatus == CL_QUEUED! !

!OpenCLEvent methodsFor: 'testing' stamp: 'jmv 9/9/2015 11:46'!
isRunning
	"Is the event currently in the 'running' state?"
	^self executionStatus == CL_RUNNING! !

!OpenCLEvent methodsFor: 'testing' stamp: 'jmv 9/9/2015 11:46'!
isSubmitted
	"Is the event currently in the 'submitted' state?"
	^self executionStatus == CL_SUBMITTED! !

!OpenCLEvent methodsFor: 'waiting' stamp: 'jcg 4/27/2010 02:05'!
wait
	"Blocking wait for this event to complete."
	OpenCLEvent waitForEvents: self.! !

!OpenCLEvent class methodsFor: 'private-waiting' stamp: 'jmv 12/23/2016 15:29:28'!
generateWaitList: eventOrListOrNil 
	"Private to the OpenCL framework.  If you call this, you're Doing It Wrong."
	"Process the argument and return a value that can be passed as the wait-list for any function that enqueues a command on a command-queue.  For convenience, this method is quite permissive... the argument may be nil, a single event, or a list of OpenCLEvents (and possibly nils).  Answers a 2-element array with: 1) the number of events 2) a CLEvent whose handle is a pointer to the concatenated handles of all events in the list (or nil if the number of events is zero)."
	| bytes events handleSize |
	eventOrListOrNil ifNil: [^#(0 nil)].
	events := eventOrListOrNil.
	events isCollection ifFalse: [events := {events}]. "easier to work with below"
	events := events select: [:evt | evt notNil]. "in case nils made it into the list"
	events size == 0 ifTrue: [^#(0 nil)].
	"If we reach here, there's at least one event that the enqueued command must wait for.
	Flatten the handles of these events into a single ByteArray, and wrap it in a CLEvent."
	handleSize := CLEvent byteSize.
	bytes := ByteArray new: (events size * handleSize).
	events withIndexDo: [:evt :i | bytes replaceFrom: (i-1*handleSize+1) to: (i*handleSize) with: evt id getHandle].
	^{events size. CLEvent new setHandle: bytes}! !

!OpenCLEvent class methodsFor: 'waiting' stamp: 'jmv 4/1/2020 22:21:59'!
waitForEvents: events "single OpenCLEvent, or list of them"
	"Blocking wait until all of the listed events are completed.  Eventually, this will be replaced with a mechanism that blocks only the current Squeak Process rather than the whole VM (see class comment of OpenCLPlugin). Note that this would require us to pin any objects passed as arguments, or disable GC (someway) until OpenCL is done..."
	| waitList result |
	waitList := OpenCLEvent generateWaitList: events.
	result := OpenCLPrimitiveInterface current 
				clWaitForEvents__cl_uint: waitList first 
							const__cl_event__ptr: waitList second.
	OpenCLError check: result text: 'failed to wait for events'.
	! !

!OpenCLMapEvent methodsFor: 'accessing' stamp: 'jcg 1/30/2010 18:13'!
pointer
	"TODO: raise error if event's command isn't finished executing."
	^pointer! !

!OpenCLMapEvent methodsFor: 'accessing' stamp: 'jcg 1/30/2010 17:36'!
rowPitch
	"Only used for images (nil for buffers)."
	^rowPitch! !

!OpenCLMapEvent methodsFor: 'accessing' stamp: 'jcg 1/30/2010 17:36'!
slicePitch
	"Only used for images (nil for buffers)."
	^slicePitch! !

!OpenCLMapEvent methodsFor: 'initialize' stamp: 'jcg 1/30/2010 18:40'!
pointer: externalData
	pointer := externalData! !

!OpenCLMapEvent methodsFor: 'initialize' stamp: 'jcg 1/30/2010 17:36'!
rowPitch: pitch
	rowPitch := pitch! !

!OpenCLMapEvent methodsFor: 'initialize' stamp: 'jcg 1/30/2010 17:36'!
slicePitch: pitch
	slicePitch := pitch! !

!OpenCLKernel methodsFor: 'private' stamp: 'jmv 1/5/2016 12:17'!
argAt: index put: arg size: byteSize
	"Please don't use. Prefer the type specific methods in category 'arguments'"

	| result | 
	result := OpenCLPrimitiveInterface current
				clSetKernelArg__cl_kernel: id
							cl_uint: index
							size_t: byteSize
							const__void__ptr: arg asOpenCLArg.
	OpenCLError check: result text: 'failed to set kernel argument ', index printString.! !

!OpenCLKernel methodsFor: 'private' stamp: 'jmv 1/5/2016 11:49'!
argAt: index putBytesOf: anObject
	"To be used with arguments other than OpenCL images and buffers.
	That is, to be used, for example with arguments declared in the OpenCL kernel as:
		int, float, double, float4, double4, etc.
	Usually prefer one of the type specific methods, such as argAt:putDouble:
	
	Takes bytes right away. No problem if openCLArg is moved or collected."

	| openCLArg result |
	openCLArg _ anObject asOpenCLArg.
	result _ OpenCLPrimitiveInterface current
				clSetKernelArg__cl_kernel: id
							cl_uint: index
							size_t: openCLArg byteSize 	"Size of the object. Number of bytes to be passed on the stack."
							const__void__ptr: openCLArg.
	OpenCLError check: result text: 'failed to set kernel argument ', index printString! !

!OpenCLKernel methodsFor: 'private' stamp: 'jcg 5/1/2010 09:15'!
privateDestroy
	program := nil.
	! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 1/5/2016 11:45'!
argAt: index putDouble4: aCollectionOfNumbers
	"To be used with arguments declared in OpenCL as double4.
	They are 4 64 bit floating point numbers together.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (Float64Array new: 4 from: aCollectionOfNumbers)! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 1/5/2016 11:46'!
argAt: index putDouble: aNumber
	"To be used with arguments declared in OpenCL as double.
	They are 64 bit floating point.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: aNumber asFloat! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 9/3/2020 17:12:45'!
argAt: index putFloat2: aCollectionOfNumbers
	"To be used with arguments declared in OpenCL as float2.
	They are 2 32 bit floating point numbers together.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (Float32Array new: 2 from: aCollectionOfNumbers)! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 9/3/2020 17:12:45'!
argAt: index putFloat4: aCollectionOfNumbers
	"To be used with arguments declared in OpenCL as float4.
	They are 4 32 bit floating point numbers together.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (Float32Array new: 4 from: aCollectionOfNumbers)! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 9/3/2020 17:12:44'!
argAt: index putFloat: aNumber
	"To be used with arguments declared in OpenCL as float.
	They are 32 bit floating point.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (Float32Array with: aNumber asFloat)! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 1/5/2016 11:46'!
argAt: index putInt: aNumber
	"To be used with arguments declared in OpenCL as int.
	They are 32 bit signed integer.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (IntegerArray with: aNumber)! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 12/26/2016 09:28:18'!
argAt: index putPointer: anOpenCLContextObject
	"To be used with arguments that are OpenCL images and buffers: OpenCLImage2D, OpenCLImage3D, OpenCLBuffer.
	That is, to be used, for example with arguments declared in the OpenCL kernel as:
		image2d_t
		float*
		etc.
	For things like float*, remember to create an OpenCLBuffer of approriate size, copy values back and forth as needed, etc.
	anOpenCLContextObject must be an OpenCLContextObject and not a regular Smalltalk object, as a Smalltalk object could
	be moved or collected, and the pointer become invalid.
	
	It could be possible to use ExternalObject. No experiments done yet, though.
	
	With a VM that supports pinned objects (Spur, someday?), thouse would be suitable too."

	| openCLArg result |
	openCLArg _ anOpenCLContextObject asOpenCLArg.
	result _ OpenCLPrimitiveInterface current
				clSetKernelArg__cl_kernel: id
							cl_uint: index
							size_t: Smalltalk wordSize 						"The size of the pointer. Number of bytes to be passed on the stack."
							const__void__ptr: openCLArg.
	OpenCLError check: result text: 'failed to set kernel argument ', index printString! !

!OpenCLKernel methodsFor: 'arguments' stamp: 'jmv 4/29/2020 09:32:59'!
argAt: index putUnsignedInt: aNumber
	"To be used with arguments declared in OpenCL as int.
	They are 32 bit unsigned integer.

	Argument to this method is a Smalltalk object. Argument passed to #argAt:putBytesOf: is also a Smalltalk object.
	Given that bytes are copied right away, there's no problem with Garbage Collection moving or reclaiming them."

	^ self argAt: index putBytesOf: (WordArray with: aNumber)! !

!OpenCLKernel methodsFor: 'accessing' stamp: 'jcg 2/15/2010 13:35'!
context
	^program context! !

!OpenCLKernel methodsFor: 'accessing' stamp: 'jcg 2/15/2010 13:35'!
program
	^program! !

!OpenCLKernel methodsFor: 'initialize' stamp: 'jcg 2/15/2010 22:01'!
program: anOpenCLProgram
	"Set the program that this kernel was created for."
	program := anOpenCLProgram.! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:09'!
initialize
	"OpenCLPrimitiveInterface initialize"
	Smalltalk addToStartUpList: self.
	Smalltalk addToShutDownList: self! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:03'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	self error: 'Unknown platform'.
	^nil! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 5/17/2017 12:32:26'!
subclassForCurrentPlatform
	"
	OpenCLPrimitiveInterface subclassForCurrentPlatform
	"
	"Explicit references to subclasses, so they are not accidentally erased, for example, by doing #reduceCuis. Add any new subclass to this list!!"
	^Smalltalk wordSize = 8
		ifTrue: [
			Smalltalk platformName caseOf: {
				[ 'unix' ] -> [ OpenCLUnixInterface ].
				[ 'Win32' ] -> [ OpenCLWindowsInterface ].
				[ 'Mac OS' ] -> [ OpenCLMacOSInterface ]
			}
			otherwise: [ self error: 'OpenCL not yet supported on your platform' ]]
		ifFalse: [
			Smalltalk platformName caseOf: {
				[ 'unix' ] -> [ OpenCLUnix32BitsInterface ].
				[ 'Win32' ] -> [ OpenCLWindows32BitsInterface ].
				[ 'Mac OS' ] -> [ OpenCLMacOS32BitsInterface ]
			}
			otherwise: [ self error: 'OpenCL not yet supported on your platform' ]]! !

!OpenCLPrimitiveInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:13'!
supportedPlatform
	^nil! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 3/6/2018 17:41:34'!
adaptType: cTypeString
	| answer |
	answer := cTypeString.
	answer := answer copyReplaceAll: 'const'  with: 'const__' .
	answer := answer copyReplaceAll: '*'  with: ' __ptr' .
	answer := answer asValidSelector.
	^ answer! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:13:19'!
clearFFICalls
	"
	OpenCLPrimitiveInterface  subclassesDo: [ :cls | cls clearFFICalls ].
	"
	self class selectors copy do: [ :sel |
		((self class whichCategoryIncludesSelector: sel) beginsWith: 'FFI-generated-')
			ifTrue: [self class removeSelector: sel ]].
	self class organization removeEmptyCategories ! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 14:40:57'!
generateFFICall: returnType name: funcName args: args cat: category

	"Compile."
	self class
		compile: (self generateMethod: funcName returnType: returnType name: funcName args: args)
		classified: ('FFI-generated-', category) 
		withStamp: nil
		notifying: nil
		logSource: true! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 13:28:02'!
generateFFICallSignatureName: funcName args: args on: stream
	"Write the method signature.  Each argument after the first gets a keyword that tells about the C type."
	"Return type in the name. currently disabled."
	" stream nextPutAll: (self adaptType: returnType); nextPutAll: '__'." 
	stream nextPutAll: funcName.
	(args size > 0) ifTrue: [stream nextPutAll: '__' ].
	args do: [ :arg |
		| argType argName |
		argType := self adaptType: arg key.
		stream nextPutAll: argType; nextPut: $:.
		argName := arg value.
		argName at: 1 put: argName first asLowercase.
		"Some are array-types (i.e 'origin[3]'), so strip off the end."
		argName := argName upTo: $[.
		"Write the name."
		stream space; nextPutAll: argName; space
	] separatedBy: [
		stream newLineTab: 6.
	]! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:51:21'!
generateFFICalls

	self readFunctionSpecsFrom: self functionDeclarationsInC do: [ :returnType :funcName :args :category |
		self generateFFICall: returnType name: funcName args: args cat: category ]! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 14:40:45'!
generateMethod: selector returnType: returnType name: funcName args: args
		
	^String streamContents: [ :stream |
		"Write the method signature."
		self generateFFICallSignatureName: selector args: args on: stream.

		"Method comment."
		stream newLine; newLine; tab; nextPutAll: '"This method was automatically generated."'; newLine.
		"C function declaration"
		stream newLine; tab; nextPut: $"; nextPutAll: returnType.
		stream space; nextPutAll: funcName; nextPut: $(.
		args do: [:arg |
			stream newLineTab: 3; nextPutAll: arg key; space; nextPutAll: arg value.
		] separatedBy: [
			stream nextPutAll: ', '.
		].
		stream newLine; tab; nextPutAll: ');"'.

		"The actual FFI declaration. About time!!"
		stream newLine; tab; nextPutAll: '<apicall: '.
		stream nextPutAll: (self typeOf: returnType).
		stream nextPutAll: ' '''; nextPutAll: funcName; nextPutAll: ''' ('.
		args do: [:arg |
			stream nextPutAll: (self typeOf: arg key)
		] separatedBy: [
			stream space
		].
		stream nextPutAll: ') module: '''; nextPutAll: self libraryForCurrentPlatform; nextPutAll: '''>'.
		"Finally, the error fall-through."
		stream newLine; newLine; tab; nextPutAll: '^self externalCallFailed'.
	]! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 14:13:13'!
opaqueHandlerTypes
	"^#()"

	^ #(
		'cl_platform_id'
		'cl_device_id'
		'cl_context'
		'cl_command_queue'
		'cl_mem'
		'cl_program'
		'cl_kernel'
		'cl_event'
		'cl_sampler')! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 12:49:15'!
readFunctionSpecsFrom: aString do: aBlock
	"self readFunctionSpecsFrom: self functionDeclarationsInC"
	| ind starts finishes funcs categories characterCount lastLineWasComment |
	
	"Find category names, and the start index for each category (so that we can
	 determine which category a function belongs."
	characterCount := 0.
	categories := OrderedCollection new.
	lastLineWasComment := false.
	aString linesDo: [:line |
		(line beginsWith: '//') 
			ifFalse: [lastLineWasComment := false]
			ifTrue: [
				lastLineWasComment ifFalse: [
					categories add: {
						(line copyFrom: 3 to: line size) withBlanksTrimmed.
						characterCount + 1.
					}.
				]..
				lastLineWasComment := true.
			].
		characterCount := characterCount + line size + 1 "for carriage-return".
	].
	categories := categories reversed.  "allows us to use #detect: below"
	
	"Find the start and end index of each function declaration."
	ind := 0.
	starts := OrderedCollection new.
	[(ind := aString indexOfSubCollection: 'extern CL_API_ENTRY ' startingAt: ind+1) isZero]
		whileFalse: [starts add: ind].
	finishes := OrderedCollection new.
	{
		' CL_API_SUFFIX__VERSION_1_0;'.
		' CL_API_SUFFIX__VERSION_1_1;'.
		' CL_API_SUFFIX__VERSION_1_2;'.
		' CL_API_SUFFIX__VERSION_2_0;'.
		' CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;'.
		' CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;' } do: [ :suffix |
			ind := 0.
			[(ind := aString indexOfSubCollection: suffix startingAt: ind+1).
			 ind isZero] 
				whileFalse: [finishes add: ind]].
	finishes sort.

	funcs := starts with: finishes collect: [:start :finish | aString copyFrom: start to: finish].
	funcs := funcs collect: [:f | f copyReplaceAll: 'extern CL_API_ENTRY' with: ''].
	funcs := funcs collect: [:f | f copyReplaceAll: 'CL_API_CALL' with: ''].
	funcs := funcs collect: [:f | f copyReplaceAll: 'CL_EXT_PREFIX__VERSION_1_1_DEPRECATED' with: ''].
	funcs := funcs collect: [:f | f copyReplaceAll: 'CL_EXT_PREFIX__VERSION_1_2_DEPRECATED' with: ''].
	
	funcs with: starts do: [ :f :start |
		| returnType funcName args stream category |
		stream := f readStream.
		stream skipSeparators.
		"Get the return type.  We're lucky that all function names begin with 'cl'.
		 (we read the first two characters separately, because they're often 'cl')"
		returnType := ((stream next: 2), (stream upToAll: 'cl')) withBlanksTrimmed.
		"Before reading the function name, we first need to replace the 
		 initial two characters that we gobbled."
		stream position: stream position - 2.
		funcName := stream upTo: $(.
		"Get a string with all of the args.  We'll still need to parse it."
		args := stream upToAll: '*/)'.
		"Get a string for each arg.  We'll still need to parse each one."
		stream := args readStream.
		args := OrderedCollection new.
		[stream atEnd] whileFalse: [args add: (stream upToAll: '*/')].
		"Parse each arg."
		args := args collect: [:a |
			| argName argType |
			stream := a readStream.
			argType := stream upToAll: '/*'.
			argName := stream upToEnd withBlanksTrimmed.
			"Clean up arg-type."
			(argType first = $,) ifTrue: [argType := argType copyFrom: 2 to: argType size].
			argType := argType withBlanksTrimmed.
			(argType beginsWith: 'void (*') ifTrue: [
				"It's a function-pointer... we need to grab the name in a special way."
				argName := ((argType copyUpTo: $) ) allButFirst: 7) withBlanksTrimmed.
			].
			argType -> argName
		].
		"Handle no-arg functions declared as clFuncName(void)."
		args size = 1 ifTrue: [
			| argName argType |
			argName := args first value.
			argType := args first key.
			(argName = '' and: [argType = 'void)']) ifTrue: [args := OrderedCollection new].
		].
		category := categories detect: [:c | c second <= start] ifNone: #('misc API').
		category := category first. "the name"
		aBlock value: returnType value: funcName value: args value: category
	]! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 11:57:07'!
typeMap
	"Answer a Dictionary that maps from OpenCL types to the corresponding Squeak FFI type.  The content of the map was manually derived from the files:
	-http://www.khronos.org/registry/cl/api/1.0/cl.h and 	-http://www.khronos.org/registry/cl/api/1.0/cl_platform.h"
	"
	OpenCLPrimitiveInterface typeMap
	"
	| input map |
	input := 
	#(
		"FFI types of kind 'handle to opaque structure'."
		('CLPlatformID' 
			('cl_platform_id'))
		('CLDeviceID' 
			('cl_device_id'))
		('CLContext' 
			('cl_context'))
		('CLCommandQueue' 
			('cl_command_queue'))
		('CLMem' 
			('cl_mem'))
		('CLProgram' 
			('cl_program'))
		('CLKernel' 
			('cl_kernel'))
		('CLEvent' 
			('cl_event'))
		('CLSampler' 
			('cl_sampler'))
		"FFI structure type."
		('CLImageFormatStruct'
			('cl_image_format'))
		('CLImageDescStruct'
			('cl_image_desc'))
		('CLBufferRegionStruct'
			('cl_buffer_region'))
		"Mappings to built-in C types."
		('void' 
			('void'))
		('void*' 
			('cl_device_partition_property' 'cl_context_properties' 'cl_pipe_properties'))
		('schar'
			('cl_char' 'char'))
		('char'
			('cl_uchar'))
		('short'
			('cl_short'))
		('ushort'
			('cl_ushort' 'cl_half'))
		('long'
			('cl_int' 'cl_build_status'))
		('ulong'
			('cl_uint' 'cl_bool' 'cl_platform_info' 'cl_device_info' 'cl_device_mem_cache_type'
			 'cl_device_local_mem_type' 'cl_context_info' 'cl_command_queue_info'
			 'cl_channel_order' 'cl_channel_type' 'cl_mem_object_type' 'cl_mem_info'
			 'cl_image_info' 'cl_addressing_mode' 'cl_filter_mode' 'cl_sampler_info'
			 'cl_program_info' 'cl_program_build_info' 'cl_kernel_info' 
			 'cl_kernel_work_group_info' 'cl_event_info' 'cl_command_type'
			 'cl_profiling_info' 'size_t'
			'cl_buffer_create_type' 'cl_pipe_info' 'cl_kernel_exec_info' 'cl_kernel_arg_info'))
		('longlong'
			('cl_long'))
		('ulonglong'
			('cl_ulong' 'cl_bitfield' 'cl_device_type' 'cl_device_address_info' 
			 'cl_device_fp_config' 'cl_device_exec_capabilities' 
			 'cl_command_queue_properties' 'cl_mem_flags' 'cl_map_flags'
			'cl_queue_properties' 'cl_svm_mem_flags' 'cl_sampler_properties'
			'cl_mem_migration_flags'))
		('float'
			('cl_float'))
		('double'
			('cl_double'))
	).
	map := Dictionary new.
	input do: [:spec |
		spec second do: [:clType | map at: clType put: spec first]
	].
	^map! !

!OpenCLPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 12/23/2016 14:40:16'!
typeOf: inputTypeName
	| typeName isPointer |
	"We'll be munging this, so assign it to a temp var."
	typeName := inputTypeName.
	"Const means nothing to us... strip it off."
	(typeName beginsWith: 'const') ifTrue: [
		typeName := (typeName allButFirst: 5) withBlanksTrimmed.
	].
	"Handle function pointers in really dumb way.... we don't support callbacks anyway,
	 so just treat them as 'void*'."
	(typeName beginsWith: 'void (*') ifTrue: [^'void*'].
	"Callers will need to know what to pass for these on a case-by-case basis."
	(typeName endsWith: 'char **') ifTrue: [^'void*'].
	(typeName endsWith: 'void **') ifTrue: [^'void*'].
	"Handle pointer types."
	(typeName last = $*) 
		ifFalse: [isPointer := false]
		ifTrue: [
			typeName := typeName allButLast withBlanksTrimmed.
			isPointer := true
		].
	"We ignore const-ness."
	(typeName beginsWith: 'const ') ifTrue: [
		typeName := typeName allButFirst: 6.
	].
	"Finally, look up the type (and make it a pointer-type if necessary)."
	typeName := self typeMap at: typeName.
	isPointer ifTrue: [typeName := typeName, '*'].
	"Oops, one last hack...FFI doesn't know what to do with void**"
	(typeName beginsWith: 'void*') ifTrue: [typeName := 'void*'].
	^typeName! !

!OpenCLPrimitiveInterface class methodsFor: 'C source code' stamp: 'jmv 12/23/2016 12:14:53'!
constantDeclarationsInC
	"Cut'n'paste directly from cl.h header file.
	Updated to OpenCL 2.0
	From https://www.khronos.org/registry/cl/api/2.0/"
^'
// Error Codes
#define CL_SUCCESS                                  0
#define CL_DEVICE_NOT_FOUND                         -1
#define CL_DEVICE_NOT_AVAILABLE                     -2
#define CL_COMPILER_NOT_AVAILABLE                   -3
#define CL_MEM_OBJECT_ALLOCATION_FAILURE            -4
#define CL_OUT_OF_RESOURCES                         -5
#define CL_OUT_OF_HOST_MEMORY                       -6
#define CL_PROFILING_INFO_NOT_AVAILABLE             -7
#define CL_MEM_COPY_OVERLAP                         -8
#define CL_IMAGE_FORMAT_MISMATCH                    -9
#define CL_IMAGE_FORMAT_NOT_SUPPORTED               -10
#define CL_BUILD_PROGRAM_FAILURE                    -11
#define CL_MAP_FAILURE                              -12
#define CL_MISALIGNED_SUB_BUFFER_OFFSET             -13
#define CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST -14
#define CL_COMPILE_PROGRAM_FAILURE                  -15
#define CL_LINKER_NOT_AVAILABLE                     -16
#define CL_LINK_PROGRAM_FAILURE                     -17
#define CL_DEVICE_PARTITION_FAILED                  -18
#define CL_KERNEL_ARG_INFO_NOT_AVAILABLE            -19

#define CL_INVALID_VALUE                            -30
#define CL_INVALID_DEVICE_TYPE                      -31
#define CL_INVALID_PLATFORM                         -32
#define CL_INVALID_DEVICE                           -33
#define CL_INVALID_CONTEXT                          -34
#define CL_INVALID_QUEUE_PROPERTIES                 -35
#define CL_INVALID_COMMAND_QUEUE                    -36
#define CL_INVALID_HOST_PTR                         -37
#define CL_INVALID_MEM_OBJECT                       -38
#define CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          -39
#define CL_INVALID_IMAGE_SIZE                       -40
#define CL_INVALID_SAMPLER                          -41
#define CL_INVALID_BINARY                           -42
#define CL_INVALID_BUILD_OPTIONS                    -43
#define CL_INVALID_PROGRAM                          -44
#define CL_INVALID_PROGRAM_EXECUTABLE               -45
#define CL_INVALID_KERNEL_NAME                      -46
#define CL_INVALID_KERNEL_DEFINITION                -47
#define CL_INVALID_KERNEL                           -48
#define CL_INVALID_ARG_INDEX                        -49
#define CL_INVALID_ARG_VALUE                        -50
#define CL_INVALID_ARG_SIZE                         -51
#define CL_INVALID_KERNEL_ARGS                      -52
#define CL_INVALID_WORK_DIMENSION                   -53
#define CL_INVALID_WORK_GROUP_SIZE                  -54
#define CL_INVALID_WORK_ITEM_SIZE                   -55
#define CL_INVALID_GLOBAL_OFFSET                    -56
#define CL_INVALID_EVENT_WAIT_LIST                  -57
#define CL_INVALID_EVENT                            -58
#define CL_INVALID_OPERATION                        -59
#define CL_INVALID_GL_OBJECT                        -60
#define CL_INVALID_BUFFER_SIZE                      -61
#define CL_INVALID_MIP_LEVEL                        -62
#define CL_INVALID_GLOBAL_WORK_SIZE                 -63
#define CL_INVALID_PROPERTY                         -64
#define CL_INVALID_IMAGE_DESCRIPTOR                 -65
#define CL_INVALID_COMPILER_OPTIONS                 -66
#define CL_INVALID_LINKER_OPTIONS                   -67
#define CL_INVALID_DEVICE_PARTITION_COUNT           -68
#define CL_INVALID_PIPE_SIZE                        -69
#define CL_INVALID_DEVICE_QUEUE                     -70

// OpenCL Version
#define CL_VERSION_1_0                              1
#define CL_VERSION_1_1                              1
#define CL_VERSION_1_2                              1
#define CL_VERSION_2_0                              1

// cl_bool
#define CL_FALSE                                    0
#define CL_TRUE                                     1
#define CL_BLOCKING                                 CL_TRUE
#define CL_NON_BLOCKING                             CL_FALSE

// cl_platform_info
#define CL_PLATFORM_PROFILE                         0x0900
#define CL_PLATFORM_VERSION                         0x0901
#define CL_PLATFORM_NAME                            0x0902
#define CL_PLATFORM_VENDOR                          0x0903
#define CL_PLATFORM_EXTENSIONS                      0x0904

// cl_device_type - bitfield
#define CL_DEVICE_TYPE_DEFAULT                      (1 << 0)
#define CL_DEVICE_TYPE_CPU                          (1 << 1)
#define CL_DEVICE_TYPE_GPU                          (1 << 2)
#define CL_DEVICE_TYPE_ACCELERATOR                  (1 << 3)
#define CL_DEVICE_TYPE_CUSTOM                       (1 << 4)
#define CL_DEVICE_TYPE_ALL                          0xFFFFFFFF

// cl_device_info
#define CL_DEVICE_TYPE                              0x1000
#define CL_DEVICE_VENDOR_ID                         0x1001
#define CL_DEVICE_MAX_COMPUTE_UNITS                 0x1002
#define CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS          0x1003
#define CL_DEVICE_MAX_WORK_GROUP_SIZE               0x1004
#define CL_DEVICE_MAX_WORK_ITEM_SIZES               0x1005
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR       0x1006
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT      0x1007
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT        0x1008
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG       0x1009
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT      0x100A
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE     0x100B
#define CL_DEVICE_MAX_CLOCK_FREQUENCY               0x100C
#define CL_DEVICE_ADDRESS_BITS                      0x100D
#define CL_DEVICE_MAX_READ_IMAGE_ARGS               0x100E
#define CL_DEVICE_MAX_WRITE_IMAGE_ARGS              0x100F
#define CL_DEVICE_MAX_MEM_ALLOC_SIZE                0x1010
#define CL_DEVICE_IMAGE2D_MAX_WIDTH                 0x1011
#define CL_DEVICE_IMAGE2D_MAX_HEIGHT                0x1012
#define CL_DEVICE_IMAGE3D_MAX_WIDTH                 0x1013
#define CL_DEVICE_IMAGE3D_MAX_HEIGHT                0x1014
#define CL_DEVICE_IMAGE3D_MAX_DEPTH                 0x1015
#define CL_DEVICE_IMAGE_SUPPORT                     0x1016
#define CL_DEVICE_MAX_PARAMETER_SIZE                0x1017
#define CL_DEVICE_MAX_SAMPLERS                      0x1018
#define CL_DEVICE_MEM_BASE_ADDR_ALIGN               0x1019
#define CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE          0x101A
#define CL_DEVICE_SINGLE_FP_CONFIG                  0x101B
#define CL_DEVICE_GLOBAL_MEM_CACHE_TYPE             0x101C
#define CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE         0x101D
#define CL_DEVICE_GLOBAL_MEM_CACHE_SIZE             0x101E
#define CL_DEVICE_GLOBAL_MEM_SIZE                   0x101F
#define CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE          0x1020
#define CL_DEVICE_MAX_CONSTANT_ARGS                 0x1021
#define CL_DEVICE_LOCAL_MEM_TYPE                    0x1022
#define CL_DEVICE_LOCAL_MEM_SIZE                    0x1023
#define CL_DEVICE_ERROR_CORRECTION_SUPPORT          0x1024
#define CL_DEVICE_PROFILING_TIMER_RESOLUTION        0x1025
#define CL_DEVICE_ENDIAN_LITTLE                     0x1026
#define CL_DEVICE_AVAILABLE                         0x1027
#define CL_DEVICE_COMPILER_AVAILABLE                0x1028
#define CL_DEVICE_EXECUTION_CAPABILITIES            0x1029
// CL_DEVICE_QUEUE_PROPERTIES is deprecated!!
#define CL_DEVICE_QUEUE_PROPERTIES                      0x102A    
#define CL_DEVICE_QUEUE_ON_HOST_PROPERTIES              0x102A
#define CL_DEVICE_NAME                              0x102B
#define CL_DEVICE_VENDOR                            0x102C
#define CL_DRIVER_VERSION                           0x102D
#define CL_DEVICE_PROFILE                           0x102E
#define CL_DEVICE_VERSION                           0x102F
#define CL_DEVICE_EXTENSIONS                        0x1030
#define CL_DEVICE_PLATFORM                          0x1031
#define CL_DEVICE_DOUBLE_FP_CONFIG                      0x1032
// 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF           0x1034
// CL_DEVICE_HOST_UNIFIED_MEMORY is deprecated!!
#define CL_DEVICE_HOST_UNIFIED_MEMORY                   0x1035
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR              0x1036
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT             0x1037
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_INT               0x1038
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG              0x1039
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT             0x103A
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE            0x103B
#define CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF              0x103C
#define CL_DEVICE_OPENCL_C_VERSION                      0x103D
#define CL_DEVICE_LINKER_AVAILABLE                      0x103E
#define CL_DEVICE_BUILT_IN_KERNELS                      0x103F
#define CL_DEVICE_IMAGE_MAX_BUFFER_SIZE                 0x1040
#define CL_DEVICE_IMAGE_MAX_ARRAY_SIZE                  0x1041
#define CL_DEVICE_PARENT_DEVICE                         0x1042
#define CL_DEVICE_PARTITION_MAX_SUB_DEVICES             0x1043
#define CL_DEVICE_PARTITION_PROPERTIES                  0x1044
#define CL_DEVICE_PARTITION_AFFINITY_DOMAIN             0x1045
#define CL_DEVICE_PARTITION_TYPE                        0x1046
#define CL_DEVICE_REFERENCE_COUNT                       0x1047
#define CL_DEVICE_PREFERRED_INTEROP_USER_SYNC           0x1048
#define CL_DEVICE_PRINTF_BUFFER_SIZE                    0x1049
#define CL_DEVICE_IMAGE_PITCH_ALIGNMENT                 0x104A
#define CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT          0x104B
#define CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS             0x104C
#define CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE              0x104D
#define CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES            0x104E
#define CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE        0x104F
#define CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE              0x1050
#define CL_DEVICE_MAX_ON_DEVICE_QUEUES                  0x1051
#define CL_DEVICE_MAX_ON_DEVICE_EVENTS                  0x1052
#define CL_DEVICE_SVM_CAPABILITIES                      0x1053
#define CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE  0x1054
#define CL_DEVICE_MAX_PIPE_ARGS                         0x1055
#define CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS          0x1056
#define CL_DEVICE_PIPE_MAX_PACKET_SIZE                  0x1057
#define CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT   0x1058
#define CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT     0x1059
#define CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT      0x105A
	
// cl_device_fp_config - bitfield
#define CL_FP_DENORM                                (1 << 0)
#define CL_FP_INF_NAN                               (1 << 1)
#define CL_FP_ROUND_TO_NEAREST                      (1 << 2)
#define CL_FP_ROUND_TO_ZERO                         (1 << 3)
#define CL_FP_ROUND_TO_INF                          (1 << 4)
#define CL_FP_FMA                                   (1 << 5)
#define CL_FP_SOFT_FLOAT                            (1 << 6)
#define CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT         (1 << 7)

// cl_device_mem_cache_type
#define CL_NONE                                     0x0
#define CL_READ_ONLY_CACHE                          0x1
#define CL_READ_WRITE_CACHE                         0x2

// cl_device_local_mem_type
#define CL_LOCAL                                    0x1
#define CL_GLOBAL                                   0x2

// cl_device_exec_capabilities - bitfield
#define CL_EXEC_KERNEL                              (1 << 0)
#define CL_EXEC_NATIVE_KERNEL                       (1 << 1)

// cl_command_queue_properties - bitfield
#define CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE      (1 << 0)
#define CL_QUEUE_PROFILING_ENABLE                   (1 << 1)
#define CL_QUEUE_ON_DEVICE                          (1 << 2)
#define CL_QUEUE_ON_DEVICE_DEFAULT                  (1 << 3)

// cl_context_info
#define CL_CONTEXT_REFERENCE_COUNT                  0x1080
#define CL_CONTEXT_DEVICES                          0x1081
#define CL_CONTEXT_PROPERTIES                       0x1082
#define CL_CONTEXT_NUM_DEVICES                      0x1083

// cl_context_properties
#define CL_CONTEXT_PLATFORM                         0x1084
#define CL_CONTEXT_INTEROP_USER_SYNC                0x1085
    
// cl_device_partition_property 
#define CL_DEVICE_PARTITION_EQUALLY                 0x1086
#define CL_DEVICE_PARTITION_BY_COUNTS               0x1087
#define CL_DEVICE_PARTITION_BY_COUNTS_LIST_END      0x0
#define CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN      0x1088
    
// cl_device_affinity_domain 
#define CL_DEVICE_AFFINITY_DOMAIN_NUMA               (1 << 0)
#define CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE           (1 << 1)
#define CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE           (1 << 2)
#define CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE           (1 << 3)
#define CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE           (1 << 4)
#define CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE (1 << 5)
    
// cl_device_svm_capabilities 
#define CL_DEVICE_SVM_COARSE_GRAIN_BUFFER           (1 << 0)
#define CL_DEVICE_SVM_FINE_GRAIN_BUFFER             (1 << 1)
#define CL_DEVICE_SVM_FINE_GRAIN_SYSTEM             (1 << 2)
#define CL_DEVICE_SVM_ATOMICS                       (1 << 3)

// cl_command_queue_info
#define CL_QUEUE_CONTEXT                            0x1090
#define CL_QUEUE_DEVICE                             0x1091
#define CL_QUEUE_REFERENCE_COUNT                    0x1092
#define CL_QUEUE_PROPERTIES                         0x1093
#define CL_QUEUE_SIZE                               0x1094

// cl_mem_flags and cl_svm_mem_flags - bitfield 
#define CL_MEM_READ_WRITE                           (1 << 0)
#define CL_MEM_WRITE_ONLY                           (1 << 1)
#define CL_MEM_READ_ONLY                            (1 << 2)
#define CL_MEM_USE_HOST_PTR                         (1 << 3)
#define CL_MEM_ALLOC_HOST_PTR                       (1 << 4)
#define CL_MEM_COPY_HOST_PTR                        (1 << 5)
// reserved                                         (1 << 6)    
#define CL_MEM_HOST_WRITE_ONLY                      (1 << 7)
#define CL_MEM_HOST_READ_ONLY                       (1 << 8)
#define CL_MEM_HOST_NO_ACCESS                       (1 << 9)
// used by cl_svm_mem_flags only
#define CL_MEM_SVM_FINE_GRAIN_BUFFER                (1 << 10)   
// used by cl_svm_mem_flags only
#define CL_MEM_SVM_ATOMICS                          (1 << 11)   
#define CL_MEM_KERNEL_READ_AND_WRITE                (1 << 12)

// cl_mem_migration_flags - bitfield
#define CL_MIGRATE_MEM_OBJECT_HOST                  (1 << 0)
#define CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED     (1 << 1)

// cl_channel_order
#define CL_R                                        0x10B0
#define CL_A                                        0x10B1
#define CL_RG                                       0x10B2
#define CL_RA                                       0x10B3
#define CL_RGB                                      0x10B4
#define CL_RGBA                                     0x10B5
#define CL_BGRA                                     0x10B6
#define CL_ARGB                                     0x10B7
#define CL_INTENSITY                                0x10B8
#define CL_LUMINANCE                                0x10B9
#define CL_Rx                                       0x10BA
#define CL_RGx                                      0x10BB
#define CL_RGBx                                     0x10BC
#define CL_DEPTH                                    0x10BD
#define CL_DEPTH_STENCIL                            0x10BE
#define CL_sRGB                                     0x10BF
#define CL_sRGBx                                    0x10C0
#define CL_sRGBA                                    0x10C1
#define CL_sBGRA                                    0x10C2
#define CL_ABGR                                     0x10C3

// cl_channel_type
#define CL_SNORM_INT8                               0x10D0
#define CL_SNORM_INT16                              0x10D1
#define CL_UNORM_INT8                               0x10D2
#define CL_UNORM_INT16                              0x10D3
#define CL_UNORM_SHORT_565                          0x10D4
#define CL_UNORM_SHORT_555                          0x10D5
#define CL_UNORM_INT_101010                         0x10D6
#define CL_SIGNED_INT8                              0x10D7
#define CL_SIGNED_INT16                             0x10D8
#define CL_SIGNED_INT32                             0x10D9
#define CL_UNSIGNED_INT8                            0x10DA
#define CL_UNSIGNED_INT16                           0x10DB
#define CL_UNSIGNED_INT32                           0x10DC
#define CL_HALF_FLOAT                               0x10DD
#define CL_FLOAT                                    0x10DE
#define CL_UNORM_INT24                              0x10DF

// cl_mem_object_type
#define CL_MEM_OBJECT_BUFFER                        0x10F0
#define CL_MEM_OBJECT_IMAGE2D                       0x10F1
#define CL_MEM_OBJECT_IMAGE3D                       0x10F2
#define CL_MEM_OBJECT_IMAGE2D_ARRAY                 0x10F3
#define CL_MEM_OBJECT_IMAGE1D                       0x10F4
#define CL_MEM_OBJECT_IMAGE1D_ARRAY                 0x10F5
#define CL_MEM_OBJECT_IMAGE1D_BUFFER                0x10F6
#define CL_MEM_OBJECT_PIPE                          0x10F7

// cl_mem_info
#define CL_MEM_TYPE                                 0x1100
#define CL_MEM_FLAGS                                0x1101
#define CL_MEM_SIZE                                 0x1102
#define CL_MEM_HOST_PTR                             0x1103
#define CL_MEM_MAP_COUNT                            0x1104
#define CL_MEM_REFERENCE_COUNT                      0x1105
#define CL_MEM_CONTEXT                              0x1106
#define CL_MEM_ASSOCIATED_MEMOBJECT                 0x1107
#define CL_MEM_OFFSET                               0x1108
#define CL_MEM_USES_SVM_POINTER                     0x1109

// cl_image_info
#define CL_IMAGE_FORMAT                             0x1110
#define CL_IMAGE_ELEMENT_SIZE                       0x1111
#define CL_IMAGE_ROW_PITCH                          0x1112
#define CL_IMAGE_SLICE_PITCH                        0x1113
#define CL_IMAGE_WIDTH                              0x1114
#define CL_IMAGE_HEIGHT                             0x1115
#define CL_IMAGE_DEPTH                              0x1116
#define CL_IMAGE_ARRAY_SIZE                         0x1117
#define CL_IMAGE_BUFFER                             0x1118
#define CL_IMAGE_NUM_MIP_LEVELS                     0x1119
#define CL_IMAGE_NUM_SAMPLES                        0x111A
    
// cl_pipe_info 
#define CL_PIPE_PACKET_SIZE                         0x1120
#define CL_PIPE_MAX_PACKETS                         0x1121

// cl_addressing_mode
#define CL_ADDRESS_NONE                             0x1130
#define CL_ADDRESS_CLAMP_TO_EDGE                    0x1131
#define CL_ADDRESS_CLAMP                            0x1132
#define CL_ADDRESS_REPEAT                           0x1133
#define CL_ADDRESS_MIRRORED_REPEAT                  0x1134

// cl_filter_mode
#define CL_FILTER_NEAREST                           0x1140
#define CL_FILTER_LINEAR                            0x1141

// cl_sampler_info
#define CL_SAMPLER_REFERENCE_COUNT                  0x1150
#define CL_SAMPLER_CONTEXT                          0x1151
#define CL_SAMPLER_NORMALIZED_COORDS                0x1152
#define CL_SAMPLER_ADDRESSING_MODE                  0x1153
#define CL_SAMPLER_FILTER_MODE                      0x1154
#define CL_SAMPLER_MIP_FILTER_MODE                  0x1155
#define CL_SAMPLER_LOD_MIN                          0x1156
#define CL_SAMPLER_LOD_MAX                          0x1157

// cl_map_flags - bitfield
#define CL_MAP_READ                                 (1 << 0)
#define CL_MAP_WRITE                                (1 << 1)
#define CL_MAP_WRITE_INVALIDATE_REGION              (1 << 2)

// cl_program_info
#define CL_PROGRAM_REFERENCE_COUNT                  0x1160
#define CL_PROGRAM_CONTEXT                          0x1161
#define CL_PROGRAM_NUM_DEVICES                      0x1162
#define CL_PROGRAM_DEVICES                          0x1163
#define CL_PROGRAM_SOURCE                           0x1164
#define CL_PROGRAM_BINARY_SIZES                     0x1165
#define CL_PROGRAM_BINARIES                         0x1166
#define CL_PROGRAM_NUM_KERNELS                      0x1167
#define CL_PROGRAM_KERNEL_NAMES                     0x1168

// cl_program_build_info
#define CL_PROGRAM_BUILD_STATUS                     0x1181
#define CL_PROGRAM_BUILD_OPTIONS                    0x1182
#define CL_PROGRAM_BUILD_LOG                        0x1183
#define CL_PROGRAM_BINARY_TYPE                      0x1184
#define CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE 0x1185
    
// cl_program_binary_type 
#define CL_PROGRAM_BINARY_TYPE_NONE                 0x0
#define CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT      0x1
#define CL_PROGRAM_BINARY_TYPE_LIBRARY              0x2
#define CL_PROGRAM_BINARY_TYPE_EXECUTABLE           0x4

// cl_build_status
#define CL_BUILD_SUCCESS                            0
#define CL_BUILD_NONE                               -1
#define CL_BUILD_ERROR                              -2
#define CL_BUILD_IN_PROGRESS                        -3

// cl_kernel_info
#define CL_KERNEL_FUNCTION_NAME                     0x1190
#define CL_KERNEL_NUM_ARGS                          0x1191
#define CL_KERNEL_REFERENCE_COUNT                   0x1192
#define CL_KERNEL_CONTEXT                           0x1193
#define CL_KERNEL_PROGRAM                           0x1194
#define CL_KERNEL_ATTRIBUTES                        0x1195

// cl_kernel_arg_info 
#define CL_KERNEL_ARG_ADDRESS_QUALIFIER             0x1196
#define CL_KERNEL_ARG_ACCESS_QUALIFIER              0x1197
#define CL_KERNEL_ARG_TYPE_NAME                     0x1198
#define CL_KERNEL_ARG_TYPE_QUALIFIER                0x1199
#define CL_KERNEL_ARG_NAME                          0x119A

// cl_kernel_arg_address_qualifier 
#define CL_KERNEL_ARG_ADDRESS_GLOBAL                0x119B
#define CL_KERNEL_ARG_ADDRESS_LOCAL                 0x119C
#define CL_KERNEL_ARG_ADDRESS_CONSTANT              0x119D
#define CL_KERNEL_ARG_ADDRESS_PRIVATE               0x119E

// cl_kernel_arg_access_qualifier 
#define CL_KERNEL_ARG_ACCESS_READ_ONLY              0x11A0
#define CL_KERNEL_ARG_ACCESS_WRITE_ONLY             0x11A1
#define CL_KERNEL_ARG_ACCESS_READ_WRITE             0x11A2
#define CL_KERNEL_ARG_ACCESS_NONE                   0x11A3
    
// cl_kernel_arg_type_qualifer 
#define CL_KERNEL_ARG_TYPE_NONE                     0
#define CL_KERNEL_ARG_TYPE_CONST                    (1 << 0)
#define CL_KERNEL_ARG_TYPE_RESTRICT                 (1 << 1)
#define CL_KERNEL_ARG_TYPE_VOLATILE                 (1 << 2)
#define CL_KERNEL_ARG_TYPE_PIPE                     (1 << 3)

// cl_kernel_work_group_info
#define CL_KERNEL_WORK_GROUP_SIZE                   0x11B0
#define CL_KERNEL_COMPILE_WORK_GROUP_SIZE           0x11B1
#define CL_KERNEL_LOCAL_MEM_SIZE                    0x11B2
#define CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE 0x11B3
#define CL_KERNEL_PRIVATE_MEM_SIZE                  0x11B4
#define CL_KERNEL_GLOBAL_WORK_SIZE                  0x11B5
    
// cl_kernel_exec_info 
#define CL_KERNEL_EXEC_INFO_SVM_PTRS                0x11B6
#define CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM   0x11B7

// cl_event_info
#define CL_EVENT_COMMAND_QUEUE                      0x11D0
#define CL_EVENT_COMMAND_TYPE                       0x11D1
#define CL_EVENT_REFERENCE_COUNT                    0x11D2
#define CL_EVENT_COMMAND_EXECUTION_STATUS           0x11D3
#define CL_EVENT_CONTEXT                            0x11D4

// cl_command_type
#define CL_COMMAND_NDRANGE_KERNEL                   0x11F0
#define CL_COMMAND_TASK                             0x11F1
#define CL_COMMAND_NATIVE_KERNEL                    0x11F2
#define CL_COMMAND_READ_BUFFER                      0x11F3
#define CL_COMMAND_WRITE_BUFFER                     0x11F4
#define CL_COMMAND_COPY_BUFFER                      0x11F5
#define CL_COMMAND_READ_IMAGE                       0x11F6
#define CL_COMMAND_WRITE_IMAGE                      0x11F7
#define CL_COMMAND_COPY_IMAGE                       0x11F8
#define CL_COMMAND_COPY_IMAGE_TO_BUFFER             0x11F9
#define CL_COMMAND_COPY_BUFFER_TO_IMAGE             0x11FA
#define CL_COMMAND_MAP_BUFFER                       0x11FB
#define CL_COMMAND_MAP_IMAGE                        0x11FC
#define CL_COMMAND_UNMAP_MEM_OBJECT                 0x11FD
#define CL_COMMAND_MARKER                           0x11FE
#define CL_COMMAND_ACQUIRE_GL_OBJECTS               0x11FF
#define CL_COMMAND_RELEASE_GL_OBJECTS               0x1200
#define CL_COMMAND_READ_BUFFER_RECT                 0x1201
#define CL_COMMAND_WRITE_BUFFER_RECT                0x1202
#define CL_COMMAND_COPY_BUFFER_RECT                 0x1203
#define CL_COMMAND_USER                             0x1204
#define CL_COMMAND_BARRIER                          0x1205
#define CL_COMMAND_MIGRATE_MEM_OBJECTS              0x1206
#define CL_COMMAND_FILL_BUFFER                      0x1207
#define CL_COMMAND_FILL_IMAGE                       0x1208
#define CL_COMMAND_SVM_FREE                         0x1209
#define CL_COMMAND_SVM_MEMCPY                       0x120A
#define CL_COMMAND_SVM_MEMFILL                      0x120B
#define CL_COMMAND_SVM_MAP                          0x120C
#define CL_COMMAND_SVM_UNMAP                        0x120D

// command execution status
#define CL_COMPLETE                                 0x0
#define CL_RUNNING                                  0x1
#define CL_SUBMITTED                                0x2
#define CL_QUEUED                                   0x3

// cl_buffer_create_type  
#define CL_BUFFER_CREATE_TYPE_REGION                0x1220
  
// cl_profiling_info
#define CL_PROFILING_COMMAND_QUEUED                 0x1280
#define CL_PROFILING_COMMAND_SUBMIT                 0x1281
#define CL_PROFILING_COMMAND_START                  0x1282
#define CL_PROFILING_COMMAND_END                    0x1283
#define CL_PROFILING_COMMAND_COMPLETE               0x1284
'! !

!OpenCLPrimitiveInterface class methodsFor: 'C source code' stamp: 'jmv 12/23/2016 12:14:57'!
functionDeclarationsInC
	"Cut'n'paste directly from cl.h header file.
	Updated to OpenCL 2.0
	From https://www.khronos.org/registry/cl/api/2.0/"
^ '
// Platform API
extern CL_API_ENTRY cl_int CL_API_CALL
clGetPlatformIDs(cl_uint          /* num_entries */,
                 cl_platform_id * /* platforms */,
                 cl_uint *        /* num_platforms */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL 
clGetPlatformInfo(cl_platform_id   /* platform */, 
                  cl_platform_info /* param_name */,
                  size_t           /* param_value_size */, 
                  void *           /* param_value */,
                  size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Device APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clGetDeviceIDs(cl_platform_id   /* platform */,
               cl_device_type   /* device_type */, 
               cl_uint          /* num_entries */, 
               cl_device_id *   /* devices */, 
               cl_uint *        /* num_devices */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetDeviceInfo(cl_device_id    /* device */,
                cl_device_info  /* param_name */, 
                size_t          /* param_value_size */, 
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clCreateSubDevices(cl_device_id                         /* in_device */,
                   const cl_device_partition_property * /* properties */,
                   cl_uint                              /* num_devices */,
                   cl_device_id *                       /* out_devices */,
                   cl_uint *                            /* num_devices_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainDevice(cl_device_id /* device */) CL_API_SUFFIX__VERSION_1_2;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseDevice(cl_device_id /* device */) CL_API_SUFFIX__VERSION_1_2;

// Context APIs  
extern CL_API_ENTRY cl_context CL_API_CALL
clCreateContext(const cl_context_properties * /* properties */,
                cl_uint                       /* num_devices */,
                const cl_device_id *          /* devices */,
                void (*pfn_notify)(const char *, const void *, size_t, void *) /* pfn_notify */,
                void *                        /* user_data */,
                cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_context CL_API_CALL
clCreateContextFromType(const cl_context_properties * /* properties */,
                        cl_device_type                /* device_type */,
                        void (*pfn_notify)(const char *, const void *, size_t, void *) /* pfn_notify */,
                        void *                        /* user_data */,
                        cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetContextInfo(cl_context         /* context */, 
                 cl_context_info    /* param_name */, 
                 size_t             /* param_value_size */, 
                 void *             /* param_value */, 
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Command Queue APIs
extern CL_API_ENTRY cl_command_queue CL_API_CALL
clCreateCommandQueueWithProperties(cl_context               /* context */,
                                   cl_device_id             /* device */,
                                   const cl_queue_properties *    /* properties */,
                                   cl_int *                 /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainCommandQueue(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseCommandQueue(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetCommandQueueInfo(cl_command_queue      /* command_queue */,
                      cl_command_queue_info /* param_name */,
                      size_t                /* param_value_size */,
                      void *                /* param_value */,
                      size_t *              /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Memory Object APIs
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateBuffer(cl_context   /* context */,
               cl_mem_flags /* flags */,
               size_t       /* size */,
               void *       /* host_ptr */,
               cl_int *     /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateSubBuffer(cl_mem                   /* buffer */,
                  cl_mem_flags             /* flags */,
                  cl_buffer_create_type    /* buffer_create_type */,
                  const void *             /* buffer_create_info */,
                  cl_int *                 /* errcode_ret */) CL_API_SUFFIX__VERSION_1_1;

extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateImage(cl_context              /* context */,
              cl_mem_flags            /* flags */,
              const cl_image_format * /* image_format */,
              const cl_image_desc *   /* image_desc */, 
              void *                  /* host_ptr */,
              cl_int *                /* errcode_ret */) CL_API_SUFFIX__VERSION_1_2;
                        
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreatePipe(cl_context                 /* context */,
             cl_mem_flags               /* flags */,
             cl_uint                    /* pipe_packet_size */,
             cl_uint                    /* pipe_max_packets */,
             const cl_pipe_properties * /* properties */,
             cl_int *                   /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;
                        
extern CL_API_ENTRY cl_int CL_API_CALL
clRetainMemObject(cl_mem /* memobj */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseMemObject(cl_mem /* memobj */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetSupportedImageFormats(cl_context           /* context */,
                           cl_mem_flags         /* flags */,
                           cl_mem_object_type   /* image_type */,
                           cl_uint              /* num_entries */,
                           cl_image_format *    /* image_formats */,
                           cl_uint *            /* num_image_formats */) CL_API_SUFFIX__VERSION_1_0;
                                    
extern CL_API_ENTRY cl_int CL_API_CALL
clGetMemObjectInfo(cl_mem           /* memobj */,
                   cl_mem_info      /* param_name */, 
                   size_t           /* param_value_size */,
                   void *           /* param_value */,
                   size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetImageInfo(cl_mem           /* image */,
               cl_image_info    /* param_name */, 
               size_t           /* param_value_size */,
               void *           /* param_value */,
               size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clGetPipeInfo(cl_mem           /* pipe */,
              cl_pipe_info     /* param_name */,
              size_t           /* param_value_size */,
              void *           /* param_value */,
              size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_2_0;
    

extern CL_API_ENTRY cl_int CL_API_CALL
clSetMemObjectDestructorCallback(cl_mem /* memobj */,
                                 void * /* no_callbacks_jmv */,
                                 void * /* user_data */)             CL_API_SUFFIX__VERSION_1_1;

// SVM Allocation APIs
extern CL_API_ENTRY void * CL_API_CALL
clSVMAlloc(cl_context       /* context */,
           cl_svm_mem_flags /* flags */,
           size_t           /* size */,
           cl_uint          /* alignment */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY void CL_API_CALL
clSVMFree(cl_context        /* context */,
          void *            /* svm_pointer */) CL_API_SUFFIX__VERSION_2_0;

// Sampler APIs
extern CL_API_ENTRY cl_sampler CL_API_CALL
clCreateSamplerWithProperties(cl_context                     /* context */,
                              const cl_sampler_properties *  /* normalized_coords */,
                              cl_int *                       /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainSampler(cl_sampler /* sampler */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseSampler(cl_sampler /* sampler */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetSamplerInfo(cl_sampler         /* sampler */,
                 cl_sampler_info    /* param_name */,
                 size_t             /* param_value_size */,
                 void *             /* param_value */,
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
// Program Object APIs
extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithSource(cl_context        /* context */,
                          cl_uint           /* count */,
                          const char **     /* strings */,
                          const size_t *    /* lengths */,
                          cl_int *          /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithBinary(cl_context                     /* context */,
                          cl_uint                        /* num_devices */,
                          const cl_device_id *           /* device_list */,
                          const size_t *                 /* lengths */,
                          const unsigned char **         /* binaries */,
                          cl_int *                       /* binary_status */,
                          cl_int *                       /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithBuiltInKernels(cl_context            /* context */,
                                  cl_uint               /* num_devices */,
                                  const cl_device_id *  /* device_list */,
                                  const char *          /* kernel_names */,
                                  cl_int *              /* errcode_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainProgram(cl_program /* program */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseProgram(cl_program /* program */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clBuildProgram(cl_program           /* program */,
               cl_uint              /* num_devices */,
               const cl_device_id * /* device_list */,
               const char *         /* options */, 
               void (*pfn_notify)(cl_program /* program */, void * /* user_data */),
               void *               /* user_data */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clCompileProgram(cl_program           /* program */,
                 cl_uint              /* num_devices */,
                 const cl_device_id * /* device_list */,
                 const char *         /* options */, 
                 cl_uint              /* num_input_headers */,
                 const cl_program *   /* input_headers */,
                 const char **        /* header_include_names */,
                 void * /* no_callbacks_jmv */,
                 void *               /* user_data */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_program CL_API_CALL
clLinkProgram(cl_context           /* context */,
              cl_uint              /* num_devices */,
              const cl_device_id * /* device_list */,
              const char *         /* options */, 
              cl_uint              /* num_input_programs */,
              const cl_program *   /* input_programs */,
              void *               /* no_callbacks_jmv */,
              void *               /* user_data */,
              cl_int *             /* errcode_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clUnloadPlatformCompiler(cl_platform_id /* platform */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetProgramInfo(cl_program         /* program */,
                 cl_program_info    /* param_name */,
                 size_t             /* param_value_size */,
                 void *             /* param_value */,
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetProgramBuildInfo(cl_program            /* program */,
                      cl_device_id          /* device */,
                      cl_program_build_info /* param_name */,
                      size_t                /* param_value_size */,
                      void *                /* param_value */,
                      size_t *              /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
// Kernel Object APIs
extern CL_API_ENTRY cl_kernel CL_API_CALL
clCreateKernel(cl_program      /* program */,
               const char *    /* kernel_name */,
               cl_int *        /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clCreateKernelsInProgram(cl_program     /* program */,
                         cl_uint        /* num_kernels */,
                         cl_kernel *    /* kernels */,
                         cl_uint *      /* num_kernels_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainKernel(cl_kernel    /* kernel */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseKernel(cl_kernel   /* kernel */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelArg(cl_kernel    /* kernel */,
               cl_uint      /* arg_index */,
               size_t       /* arg_size */,
               const void * /* arg_value */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelArgSVMPointer(cl_kernel    /* kernel */,
                         cl_uint      /* arg_index */,
                         const void * /* arg_value */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelExecInfo(cl_kernel            /* kernel */,
                    cl_kernel_exec_info  /* param_name */,
                    size_t               /* param_value_size */,
                    const void *         /* param_value */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelInfo(cl_kernel       /* kernel */,
                cl_kernel_info  /* param_name */,
                size_t          /* param_value_size */,
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelArgInfo(cl_kernel       /* kernel */,
                   cl_uint         /* arg_indx */,
                   cl_kernel_arg_info  /* param_name */,
                   size_t          /* param_value_size */,
                   void *          /* param_value */,
                   size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelWorkGroupInfo(cl_kernel                  /* kernel */,
                         cl_device_id               /* device */,
                         cl_kernel_work_group_info  /* param_name */,
                         size_t                     /* param_value_size */,
                         void *                     /* param_value */,
                         size_t *                   /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Event Object APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clWaitForEvents(cl_uint             /* num_events */,
                const cl_event *    /* event_list */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetEventInfo(cl_event         /* event */,
               cl_event_info    /* param_name */,
               size_t           /* param_value_size */,
               void *           /* param_value */,
               size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_event CL_API_CALL
clCreateUserEvent(cl_context    /* context */,
                  cl_int *      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_1;               
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clRetainEvent(cl_event /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseEvent(cl_event /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetUserEventStatus(cl_event   /* event */,
                     cl_int     /* execution_status */) CL_API_SUFFIX__VERSION_1_1;
                     
extern CL_API_ENTRY cl_int CL_API_CALL
clSetEventCallback( cl_event    /* event */,
                    cl_int      /* command_exec_callback_type */,
                    void *      /* no_callbacks_jmv */,
                    void *      /* user_data */) CL_API_SUFFIX__VERSION_1_1;

// Profiling APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clGetEventProfilingInfo(cl_event            /* event */,
                        cl_profiling_info   /* param_name */,
                        size_t              /* param_value_size */,
                        void *              /* param_value */,
                        size_t *            /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                                
// Flush and Finish APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clFlush(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clFinish(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

// Enqueued Commands APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadBuffer(cl_command_queue    /* command_queue */,
                    cl_mem              /* buffer */,
                    cl_bool             /* blocking_read */,
                    size_t              /* offset */,
                    size_t              /* size */, 
                    void *              /* ptr */,
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadBufferRect(cl_command_queue    /* command_queue */,
                        cl_mem              /* buffer */,
                        cl_bool             /* blocking_read */,
                        const size_t *      /* buffer_offset */,
                        const size_t *      /* host_offset */, 
                        const size_t *      /* region */,
                        size_t              /* buffer_row_pitch */,
                        size_t              /* buffer_slice_pitch */,
                        size_t              /* host_row_pitch */,
                        size_t              /* host_slice_pitch */,                        
                        void *              /* ptr */,
                        cl_uint             /* num_events_in_wait_list */,
                        const cl_event *    /* event_wait_list */,
                        cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteBuffer(cl_command_queue   /* command_queue */, 
                     cl_mem             /* buffer */, 
                     cl_bool            /* blocking_write */, 
                     size_t             /* offset */, 
                     size_t             /* size */, 
                     const void *       /* ptr */, 
                     cl_uint            /* num_events_in_wait_list */, 
                     const cl_event *   /* event_wait_list */, 
                     cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteBufferRect(cl_command_queue    /* command_queue */,
                         cl_mem              /* buffer */,
                         cl_bool             /* blocking_write */,
                         const size_t *      /* buffer_offset */,
                         const size_t *      /* host_offset */, 
                         const size_t *      /* region */,
                         size_t              /* buffer_row_pitch */,
                         size_t              /* buffer_slice_pitch */,
                         size_t              /* host_row_pitch */,
                         size_t              /* host_slice_pitch */,                        
                         const void *        /* ptr */,
                         cl_uint             /* num_events_in_wait_list */,
                         const cl_event *    /* event_wait_list */,
                         cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueFillBuffer(cl_command_queue   /* command_queue */,
                    cl_mem             /* buffer */, 
                    const void *       /* pattern */, 
                    size_t             /* pattern_size */, 
                    size_t             /* offset */, 
                    size_t             /* size */, 
                    cl_uint            /* num_events_in_wait_list */, 
                    const cl_event *   /* event_wait_list */, 
                    cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_2;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBuffer(cl_command_queue    /* command_queue */, 
                    cl_mem              /* src_buffer */,
                    cl_mem              /* dst_buffer */, 
                    size_t              /* src_offset */,
                    size_t              /* dst_offset */,
                    size_t              /* size */, 
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBufferRect(cl_command_queue    /* command_queue */, 
                        cl_mem              /* src_buffer */,
                        cl_mem              /* dst_buffer */, 
                        const size_t *      /* src_origin */,
                        const size_t *      /* dst_origin */,
                        const size_t *      /* region */, 
                        size_t              /* src_row_pitch */,
                        size_t              /* src_slice_pitch */,
                        size_t              /* dst_row_pitch */,
                        size_t              /* dst_slice_pitch */,
                        cl_uint             /* num_events_in_wait_list */,
                        const cl_event *    /* event_wait_list */,
                        cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadImage(cl_command_queue     /* command_queue */,
                   cl_mem               /* image */,
                   cl_bool              /* blocking_read */, 
                   const size_t *       /* origin[3] */,
                   const size_t *       /* region[3] */,
                   size_t               /* row_pitch */,
                   size_t               /* slice_pitch */, 
                   void *               /* ptr */,
                   cl_uint              /* num_events_in_wait_list */,
                   const cl_event *     /* event_wait_list */,
                   cl_event *           /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteImage(cl_command_queue    /* command_queue */,
                    cl_mem              /* image */,
                    cl_bool             /* blocking_write */, 
                    const size_t *      /* origin[3] */,
                    const size_t *      /* region[3] */,
                    size_t              /* input_row_pitch */,
                    size_t              /* input_slice_pitch */, 
                    const void *        /* ptr */,
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueFillImage(cl_command_queue   /* command_queue */,
                   cl_mem             /* image */, 
                   const void *       /* fill_color */, 
                   const size_t *     /* origin[3] */, 
                   const size_t *     /* region[3] */, 
                   cl_uint            /* num_events_in_wait_list */, 
                   const cl_event *   /* event_wait_list */, 
                   cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyImage(cl_command_queue     /* command_queue */,
                   cl_mem               /* src_image */,
                   cl_mem               /* dst_image */, 
                   const size_t *       /* src_origin[3] */,
                   const size_t *       /* dst_origin[3] */,
                   const size_t *       /* region[3] */, 
                   cl_uint              /* num_events_in_wait_list */,
                   const cl_event *     /* event_wait_list */,
                   cl_event *           /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyImageToBuffer(cl_command_queue /* command_queue */,
                           cl_mem           /* src_image */,
                           cl_mem           /* dst_buffer */, 
                           const size_t *   /* src_origin[3] */,
                           const size_t *   /* region[3] */, 
                           size_t           /* dst_offset */,
                           cl_uint          /* num_events_in_wait_list */,
                           const cl_event * /* event_wait_list */,
                           cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBufferToImage(cl_command_queue /* command_queue */,
                           cl_mem           /* src_buffer */,
                           cl_mem           /* dst_image */, 
                           size_t           /* src_offset */,
                           const size_t *   /* dst_origin[3] */,
                           const size_t *   /* region[3] */, 
                           cl_uint          /* num_events_in_wait_list */,
                           const cl_event * /* event_wait_list */,
                           cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY void * CL_API_CALL
clEnqueueMapBuffer(cl_command_queue /* command_queue */,
                   cl_mem           /* buffer */,
                   cl_bool          /* blocking_map */, 
                   cl_map_flags     /* map_flags */,
                   size_t           /* offset */,
                   size_t           /* size */,
                   cl_uint          /* num_events_in_wait_list */,
                   const cl_event * /* event_wait_list */,
                   cl_event *       /* event */,
                   cl_int *         /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY void * CL_API_CALL
clEnqueueMapImage(cl_command_queue  /* command_queue */,
                  cl_mem            /* image */, 
                  cl_bool           /* blocking_map */, 
                  cl_map_flags      /* map_flags */, 
                  const size_t *    /* origin[3] */,
                  const size_t *    /* region[3] */,
                  size_t *          /* image_row_pitch */,
                  size_t *          /* image_slice_pitch */,
                  cl_uint           /* num_events_in_wait_list */,
                  const cl_event *  /* event_wait_list */,
                  cl_event *        /* event */,
                  cl_int *          /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueUnmapMemObject(cl_command_queue /* command_queue */,
                        cl_mem           /* memobj */,
                        void *           /* mapped_ptr */,
                        cl_uint          /* num_events_in_wait_list */,
                        const cl_event *  /* event_wait_list */,
                        cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueMigrateMemObjects(cl_command_queue       /* command_queue */,
                           cl_uint                /* num_mem_objects */,
                           const cl_mem *         /* mem_objects */,
                           cl_mem_migration_flags /* flags */,
                           cl_uint                /* num_events_in_wait_list */,
                           const cl_event *       /* event_wait_list */,
                           cl_event *             /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueNDRangeKernel(cl_command_queue /* command_queue */,
                       cl_kernel        /* kernel */,
                       cl_uint          /* work_dim */,
                       const size_t *   /* global_work_offset */,
                       const size_t *   /* global_work_size */,
                       const size_t *   /* local_work_size */,
                       cl_uint          /* num_events_in_wait_list */,
                       const cl_event * /* event_wait_list */,
                       cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueNativeKernel(cl_command_queue  /* command_queue */,
					  void (*user_func)(void *), 
                      void *            /* args */,
                      size_t            /* cb_args */, 
                      cl_uint           /* num_mem_objects */,
                      const cl_mem *    /* mem_list */,
                      const void **     /* args_mem_loc */,
                      cl_uint           /* num_events_in_wait_list */,
                      const cl_event *  /* event_wait_list */,
                      cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueMarkerWithWaitList(cl_command_queue  /* command_queue */,
                            cl_uint           /* num_events_in_wait_list */,
                            const cl_event *  /* event_wait_list */,
                            cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueBarrierWithWaitList(cl_command_queue  /* command_queue */,
                             cl_uint           /* num_events_in_wait_list */,
                             const cl_event *  /* event_wait_list */,
                             cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMFree(cl_command_queue  /* command_queue */,
                 cl_uint           /* num_svm_pointers */,
                 void *          /* svm_pointers[] */,
                 void *           /* no_callbacks_jmv */,
                 void *            /* user_data */,
                 cl_uint           /* num_events_in_wait_list */,
                 const cl_event *  /* event_wait_list */,
                 cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMemcpy(cl_command_queue  /* command_queue */,
                   cl_bool           /* blocking_copy */,
                   void *            /* dst_ptr */,
                   const void *      /* src_ptr */,
                   size_t            /* size */,
                   cl_uint           /* num_events_in_wait_list */,
                   const cl_event *  /* event_wait_list */,
                   cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMemFill(cl_command_queue  /* command_queue */,
                    void *            /* svm_ptr */,
                    const void *      /* pattern */,
                    size_t            /* pattern_size */,
                    size_t            /* size */,
                    cl_uint           /* num_events_in_wait_list */,
                    const cl_event *  /* event_wait_list */,
                    cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMap(cl_command_queue  /* command_queue */,
                cl_bool           /* blocking_map */,
                cl_map_flags      /* flags */,
                void *            /* svm_ptr */,
                size_t            /* size */,
                cl_uint           /* num_events_in_wait_list */,
                const cl_event *  /* event_wait_list */,
                cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMUnmap(cl_command_queue  /* command_queue */,
                  void *            /* svm_ptr */,
                  cl_uint           /* num_events_in_wait_list */,
                  const cl_event *  /* event_wait_list */,
                  cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

// Extension function access
//
// Returns the extension function address for the given function name,
// or NULL if a valid function can not be found.  The client must
// check to make sure the address is not NULL, before using or 
// calling the returned function address.
//
extern CL_API_ENTRY void * CL_API_CALL 
clGetExtensionFunctionAddressForPlatform(cl_platform_id /* platform */,
                                         const char *   /* func_name */) CL_API_SUFFIX__VERSION_1_2;

// Deprecated OpenCL 1.1 APIs
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_mem CL_API_CALL
clCreateImage2D(cl_context              /* context */,
                cl_mem_flags            /* flags */,
                const cl_image_format * /* image_format */,
                size_t                  /* image_width */,
                size_t                  /* image_height */,
                size_t                  /* image_row_pitch */, 
                void *                  /* host_ptr */,
                cl_int *                /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_mem CL_API_CALL
clCreateImage3D(cl_context              /* context */,
                cl_mem_flags            /* flags */,
                const cl_image_format * /* image_format */,
                size_t                  /* image_width */, 
                size_t                  /* image_height */,
                size_t                  /* image_depth */, 
                size_t                  /* image_row_pitch */, 
                size_t                  /* image_slice_pitch */, 
                void *                  /* host_ptr */,
                cl_int *                /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueMarker(cl_command_queue    /* command_queue */,
                cl_event *          /* event */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueWaitForEvents(cl_command_queue /* command_queue */,
                        cl_uint          /* num_events */,
                        const cl_event * /* event_list */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueBarrier(cl_command_queue /* command_queue */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;

extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clUnloadCompiler(void) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED void * CL_API_CALL
clGetExtensionFunctionAddress(const char * /* func_name */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
// Deprecated OpenCL 2.0 APIs
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_command_queue CL_API_CALL
clCreateCommandQueue(cl_context                     /* context */,
                     cl_device_id                   /* device */,
                     cl_command_queue_properties    /* properties */,
                     cl_int *                       /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
    
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_sampler CL_API_CALL
clCreateSampler(cl_context          /* context */,
                cl_bool             /* normalized_coords */,
                cl_addressing_mode  /* addressing_mode */,
                cl_filter_mode      /* filter_mode */,
                cl_int *            /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_int CL_API_CALL
clEnqueueTask(cl_command_queue  /* command_queue */,
              cl_kernel         /* kernel */,
              cl_uint           /* num_events_in_wait_list */,
              const cl_event *  /* event_wait_list */,
              cl_event *        /* event */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
'
! !

!OpenCLPrimitiveInterface class methodsFor: 'C source code' stamp: 'jmv 12/23/2016 12:15:01'!
functionDeclarationsInC_withUnsupportedCallbacks
	"Cut'n'paste directly from cl.h header file.
	Updated to OpenCL 2.0
	From https://www.khronos.org/registry/cl/api/2.0/"
^ '
// Platform API
extern CL_API_ENTRY cl_int CL_API_CALL
clGetPlatformIDs(cl_uint          /* num_entries */,
                 cl_platform_id * /* platforms */,
                 cl_uint *        /* num_platforms */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL 
clGetPlatformInfo(cl_platform_id   /* platform */, 
                  cl_platform_info /* param_name */,
                  size_t           /* param_value_size */, 
                  void *           /* param_value */,
                  size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Device APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clGetDeviceIDs(cl_platform_id   /* platform */,
               cl_device_type   /* device_type */, 
               cl_uint          /* num_entries */, 
               cl_device_id *   /* devices */, 
               cl_uint *        /* num_devices */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetDeviceInfo(cl_device_id    /* device */,
                cl_device_info  /* param_name */, 
                size_t          /* param_value_size */, 
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clCreateSubDevices(cl_device_id                         /* in_device */,
                   const cl_device_partition_property * /* properties */,
                   cl_uint                              /* num_devices */,
                   cl_device_id *                       /* out_devices */,
                   cl_uint *                            /* num_devices_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainDevice(cl_device_id /* device */) CL_API_SUFFIX__VERSION_1_2;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseDevice(cl_device_id /* device */) CL_API_SUFFIX__VERSION_1_2;

// Context APIs  
extern CL_API_ENTRY cl_context CL_API_CALL
clCreateContext(const cl_context_properties * /* properties */,
                cl_uint                       /* num_devices */,
                const cl_device_id *          /* devices */,
                void (*pfn_notify)(const char *, const void *, size_t, void *) /* pfn_notify */,
                void *                        /* user_data */,
                cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_context CL_API_CALL
clCreateContextFromType(const cl_context_properties * /* properties */,
                        cl_device_type                /* device_type */,
                        void (*pfn_notify)(const char *, const void *, size_t, void *) /* pfn_notify */,
                        void *                        /* user_data */,
                        cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetContextInfo(cl_context         /* context */, 
                 cl_context_info    /* param_name */, 
                 size_t             /* param_value_size */, 
                 void *             /* param_value */, 
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Command Queue APIs
extern CL_API_ENTRY cl_command_queue CL_API_CALL
clCreateCommandQueueWithProperties(cl_context               /* context */,
                                   cl_device_id             /* device */,
                                   const cl_queue_properties *    /* properties */,
                                   cl_int *                 /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainCommandQueue(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseCommandQueue(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetCommandQueueInfo(cl_command_queue      /* command_queue */,
                      cl_command_queue_info /* param_name */,
                      size_t                /* param_value_size */,
                      void *                /* param_value */,
                      size_t *              /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Memory Object APIs
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateBuffer(cl_context   /* context */,
               cl_mem_flags /* flags */,
               size_t       /* size */,
               void *       /* host_ptr */,
               cl_int *     /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateSubBuffer(cl_mem                   /* buffer */,
                  cl_mem_flags             /* flags */,
                  cl_buffer_create_type    /* buffer_create_type */,
                  const void *             /* buffer_create_info */,
                  cl_int *                 /* errcode_ret */) CL_API_SUFFIX__VERSION_1_1;

extern CL_API_ENTRY cl_mem CL_API_CALL
clCreateImage(cl_context              /* context */,
              cl_mem_flags            /* flags */,
              const cl_image_format * /* image_format */,
              const cl_image_desc *   /* image_desc */, 
              void *                  /* host_ptr */,
              cl_int *                /* errcode_ret */) CL_API_SUFFIX__VERSION_1_2;
                        
extern CL_API_ENTRY cl_mem CL_API_CALL
clCreatePipe(cl_context                 /* context */,
             cl_mem_flags               /* flags */,
             cl_uint                    /* pipe_packet_size */,
             cl_uint                    /* pipe_max_packets */,
             const cl_pipe_properties * /* properties */,
             cl_int *                   /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;
                        
extern CL_API_ENTRY cl_int CL_API_CALL
clRetainMemObject(cl_mem /* memobj */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseMemObject(cl_mem /* memobj */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetSupportedImageFormats(cl_context           /* context */,
                           cl_mem_flags         /* flags */,
                           cl_mem_object_type   /* image_type */,
                           cl_uint              /* num_entries */,
                           cl_image_format *    /* image_formats */,
                           cl_uint *            /* num_image_formats */) CL_API_SUFFIX__VERSION_1_0;
                                    
extern CL_API_ENTRY cl_int CL_API_CALL
clGetMemObjectInfo(cl_mem           /* memobj */,
                   cl_mem_info      /* param_name */, 
                   size_t           /* param_value_size */,
                   void *           /* param_value */,
                   size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetImageInfo(cl_mem           /* image */,
               cl_image_info    /* param_name */, 
               size_t           /* param_value_size */,
               void *           /* param_value */,
               size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clGetPipeInfo(cl_mem           /* pipe */,
              cl_pipe_info     /* param_name */,
              size_t           /* param_value_size */,
              void *           /* param_value */,
              size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_2_0;
    

extern CL_API_ENTRY cl_int CL_API_CALL
clSetMemObjectDestructorCallback(cl_mem /* memobj */,
                                 void (CL_CALLBACK * /*pfn_notify*/)( cl_mem /* memobj */, void* /*user_data*/),
                                 void * /*user_data */ )             CL_API_SUFFIX__VERSION_1_1;

/* SVM Allocation APIs */
extern CL_API_ENTRY void * CL_API_CALL
clSVMAlloc(cl_context       /* context */,
           cl_svm_mem_flags /* flags */,
           size_t           /* size */,
           cl_uint          /* alignment */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY void CL_API_CALL
clSVMFree(cl_context        /* context */,
          void *            /* svm_pointer */) CL_API_SUFFIX__VERSION_2_0;

// Sampler APIs
extern CL_API_ENTRY cl_sampler CL_API_CALL
clCreateSamplerWithProperties(cl_context                     /* context */,
                              const cl_sampler_properties *  /* normalized_coords */,
                              cl_int *                       /* errcode_ret */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainSampler(cl_sampler /* sampler */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseSampler(cl_sampler /* sampler */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetSamplerInfo(cl_sampler         /* sampler */,
                 cl_sampler_info    /* param_name */,
                 size_t             /* param_value_size */,
                 void *             /* param_value */,
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
// Program Object APIs
extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithSource(cl_context        /* context */,
                          cl_uint           /* count */,
                          const char **     /* strings */,
                          const size_t *    /* lengths */,
                          cl_int *          /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithBinary(cl_context                     /* context */,
                          cl_uint                        /* num_devices */,
                          const cl_device_id *           /* device_list */,
                          const size_t *                 /* lengths */,
                          const unsigned char **         /* binaries */,
                          cl_int *                       /* binary_status */,
                          cl_int *                       /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_program CL_API_CALL
clCreateProgramWithBuiltInKernels(cl_context            /* context */,
                                  cl_uint               /* num_devices */,
                                  const cl_device_id *  /* device_list */,
                                  const char *          /* kernel_names */,
                                  cl_int *              /* errcode_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainProgram(cl_program /* program */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseProgram(cl_program /* program */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clBuildProgram(cl_program           /* program */,
               cl_uint              /* num_devices */,
               const cl_device_id * /* device_list */,
               const char *         /* options */, 
               void (*pfn_notify)(cl_program /* program */, void * /* user_data */),
               void *               /* user_data */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clCompileProgram(cl_program           /* program */,
                 cl_uint              /* num_devices */,
                 const cl_device_id * /* device_list */,
                 const char *         /* options */, 
                 cl_uint              /* num_input_headers */,
                 const cl_program *   /* input_headers */,
                 const char **        /* header_include_names */,
                 void (CL_CALLBACK *  /* pfn_notify */)(cl_program /* program */, void * /* user_data */),
                 void *               /* user_data */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_program CL_API_CALL
clLinkProgram(cl_context           /* context */,
              cl_uint              /* num_devices */,
              const cl_device_id * /* device_list */,
              const char *         /* options */, 
              cl_uint              /* num_input_programs */,
              const cl_program *   /* input_programs */,
              void (CL_CALLBACK *  /* pfn_notify */)(cl_program /* program */, void * /* user_data */),
              void *               /* user_data */,
              cl_int *             /* errcode_ret */ ) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clUnloadPlatformCompiler(cl_platform_id /* platform */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetProgramInfo(cl_program         /* program */,
                 cl_program_info    /* param_name */,
                 size_t             /* param_value_size */,
                 void *             /* param_value */,
                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetProgramBuildInfo(cl_program            /* program */,
                      cl_device_id          /* device */,
                      cl_program_build_info /* param_name */,
                      size_t                /* param_value_size */,
                      void *                /* param_value */,
                      size_t *              /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
// Kernel Object APIs
extern CL_API_ENTRY cl_kernel CL_API_CALL
clCreateKernel(cl_program      /* program */,
               const char *    /* kernel_name */,
               cl_int *        /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clCreateKernelsInProgram(cl_program     /* program */,
                         cl_uint        /* num_kernels */,
                         cl_kernel *    /* kernels */,
                         cl_uint *      /* num_kernels_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clRetainKernel(cl_kernel    /* kernel */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseKernel(cl_kernel   /* kernel */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelArg(cl_kernel    /* kernel */,
               cl_uint      /* arg_index */,
               size_t       /* arg_size */,
               const void * /* arg_value */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelArgSVMPointer(cl_kernel    /* kernel */,
                         cl_uint      /* arg_index */,
                         const void * /* arg_value */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetKernelExecInfo(cl_kernel            /* kernel */,
                    cl_kernel_exec_info  /* param_name */,
                    size_t               /* param_value_size */,
                    const void *         /* param_value */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelInfo(cl_kernel       /* kernel */,
                cl_kernel_info  /* param_name */,
                size_t          /* param_value_size */,
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelArgInfo(cl_kernel       /* kernel */,
                   cl_uint         /* arg_indx */,
                   cl_kernel_arg_info  /* param_name */,
                   size_t          /* param_value_size */,
                   void *          /* param_value */,
                   size_t *        /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetKernelWorkGroupInfo(cl_kernel                  /* kernel */,
                         cl_device_id               /* device */,
                         cl_kernel_work_group_info  /* param_name */,
                         size_t                     /* param_value_size */,
                         void *                     /* param_value */,
                         size_t *                   /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

// Event Object APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clWaitForEvents(cl_uint             /* num_events */,
                const cl_event *    /* event_list */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clGetEventInfo(cl_event         /* event */,
               cl_event_info    /* param_name */,
               size_t           /* param_value_size */,
               void *           /* param_value */,
               size_t *         /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_event CL_API_CALL
clCreateUserEvent(cl_context    /* context */,
                  cl_int *      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_1;               
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clRetainEvent(cl_event /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clReleaseEvent(cl_event /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clSetUserEventStatus(cl_event   /* event */,
                     cl_int     /* execution_status */) CL_API_SUFFIX__VERSION_1_1;
                     
extern CL_API_ENTRY cl_int CL_API_CALL
clSetEventCallback( cl_event    /* event */,
                    cl_int      /* command_exec_callback_type */,
                    void (CL_CALLBACK * /* pfn_notify */)(cl_event, cl_int, void *),
                    void *      /* user_data */) CL_API_SUFFIX__VERSION_1_1;

// Profiling APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clGetEventProfilingInfo(cl_event            /* event */,
                        cl_profiling_info   /* param_name */,
                        size_t              /* param_value_size */,
                        void *              /* param_value */,
                        size_t *            /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;
                                
// Flush and Finish APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clFlush(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clFinish(cl_command_queue /* command_queue */) CL_API_SUFFIX__VERSION_1_0;

// Enqueued Commands APIs
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadBuffer(cl_command_queue    /* command_queue */,
                    cl_mem              /* buffer */,
                    cl_bool             /* blocking_read */,
                    size_t              /* offset */,
                    size_t              /* size */, 
                    void *              /* ptr */,
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadBufferRect(cl_command_queue    /* command_queue */,
                        cl_mem              /* buffer */,
                        cl_bool             /* blocking_read */,
                        const size_t *      /* buffer_offset */,
                        const size_t *      /* host_offset */, 
                        const size_t *      /* region */,
                        size_t              /* buffer_row_pitch */,
                        size_t              /* buffer_slice_pitch */,
                        size_t              /* host_row_pitch */,
                        size_t              /* host_slice_pitch */,                        
                        void *              /* ptr */,
                        cl_uint             /* num_events_in_wait_list */,
                        const cl_event *    /* event_wait_list */,
                        cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteBuffer(cl_command_queue   /* command_queue */, 
                     cl_mem             /* buffer */, 
                     cl_bool            /* blocking_write */, 
                     size_t             /* offset */, 
                     size_t             /* size */, 
                     const void *       /* ptr */, 
                     cl_uint            /* num_events_in_wait_list */, 
                     const cl_event *   /* event_wait_list */, 
                     cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteBufferRect(cl_command_queue    /* command_queue */,
                         cl_mem              /* buffer */,
                         cl_bool             /* blocking_write */,
                         const size_t *      /* buffer_offset */,
                         const size_t *      /* host_offset */, 
                         const size_t *      /* region */,
                         size_t              /* buffer_row_pitch */,
                         size_t              /* buffer_slice_pitch */,
                         size_t              /* host_row_pitch */,
                         size_t              /* host_slice_pitch */,                        
                         const void *        /* ptr */,
                         cl_uint             /* num_events_in_wait_list */,
                         const cl_event *    /* event_wait_list */,
                         cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueFillBuffer(cl_command_queue   /* command_queue */,
                    cl_mem             /* buffer */, 
                    const void *       /* pattern */, 
                    size_t             /* pattern_size */, 
                    size_t             /* offset */, 
                    size_t             /* size */, 
                    cl_uint            /* num_events_in_wait_list */, 
                    const cl_event *   /* event_wait_list */, 
                    cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_2;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBuffer(cl_command_queue    /* command_queue */, 
                    cl_mem              /* src_buffer */,
                    cl_mem              /* dst_buffer */, 
                    size_t              /* src_offset */,
                    size_t              /* dst_offset */,
                    size_t              /* size */, 
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBufferRect(cl_command_queue    /* command_queue */, 
                        cl_mem              /* src_buffer */,
                        cl_mem              /* dst_buffer */, 
                        const size_t *      /* src_origin */,
                        const size_t *      /* dst_origin */,
                        const size_t *      /* region */, 
                        size_t              /* src_row_pitch */,
                        size_t              /* src_slice_pitch */,
                        size_t              /* dst_row_pitch */,
                        size_t              /* dst_slice_pitch */,
                        cl_uint             /* num_events_in_wait_list */,
                        const cl_event *    /* event_wait_list */,
                        cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_1;
                            
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueReadImage(cl_command_queue     /* command_queue */,
                   cl_mem               /* image */,
                   cl_bool              /* blocking_read */, 
                   const size_t *       /* origin[3] */,
                   const size_t *       /* region[3] */,
                   size_t               /* row_pitch */,
                   size_t               /* slice_pitch */, 
                   void *               /* ptr */,
                   cl_uint              /* num_events_in_wait_list */,
                   const cl_event *     /* event_wait_list */,
                   cl_event *           /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueWriteImage(cl_command_queue    /* command_queue */,
                    cl_mem              /* image */,
                    cl_bool             /* blocking_write */, 
                    const size_t *      /* origin[3] */,
                    const size_t *      /* region[3] */,
                    size_t              /* input_row_pitch */,
                    size_t              /* input_slice_pitch */, 
                    const void *        /* ptr */,
                    cl_uint             /* num_events_in_wait_list */,
                    const cl_event *    /* event_wait_list */,
                    cl_event *          /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueFillImage(cl_command_queue   /* command_queue */,
                   cl_mem             /* image */, 
                   const void *       /* fill_color */, 
                   const size_t *     /* origin[3] */, 
                   const size_t *     /* region[3] */, 
                   cl_uint            /* num_events_in_wait_list */, 
                   const cl_event *   /* event_wait_list */, 
                   cl_event *         /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyImage(cl_command_queue     /* command_queue */,
                   cl_mem               /* src_image */,
                   cl_mem               /* dst_image */, 
                   const size_t *       /* src_origin[3] */,
                   const size_t *       /* dst_origin[3] */,
                   const size_t *       /* region[3] */, 
                   cl_uint              /* num_events_in_wait_list */,
                   const cl_event *     /* event_wait_list */,
                   cl_event *           /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyImageToBuffer(cl_command_queue /* command_queue */,
                           cl_mem           /* src_image */,
                           cl_mem           /* dst_buffer */, 
                           const size_t *   /* src_origin[3] */,
                           const size_t *   /* region[3] */, 
                           size_t           /* dst_offset */,
                           cl_uint          /* num_events_in_wait_list */,
                           const cl_event * /* event_wait_list */,
                           cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueCopyBufferToImage(cl_command_queue /* command_queue */,
                           cl_mem           /* src_buffer */,
                           cl_mem           /* dst_image */, 
                           size_t           /* src_offset */,
                           const size_t *   /* dst_origin[3] */,
                           const size_t *   /* region[3] */, 
                           cl_uint          /* num_events_in_wait_list */,
                           const cl_event * /* event_wait_list */,
                           cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY void * CL_API_CALL
clEnqueueMapBuffer(cl_command_queue /* command_queue */,
                   cl_mem           /* buffer */,
                   cl_bool          /* blocking_map */, 
                   cl_map_flags     /* map_flags */,
                   size_t           /* offset */,
                   size_t           /* size */,
                   cl_uint          /* num_events_in_wait_list */,
                   const cl_event * /* event_wait_list */,
                   cl_event *       /* event */,
                   cl_int *         /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY void * CL_API_CALL
clEnqueueMapImage(cl_command_queue  /* command_queue */,
                  cl_mem            /* image */, 
                  cl_bool           /* blocking_map */, 
                  cl_map_flags      /* map_flags */, 
                  const size_t *    /* origin[3] */,
                  const size_t *    /* region[3] */,
                  size_t *          /* image_row_pitch */,
                  size_t *          /* image_slice_pitch */,
                  cl_uint           /* num_events_in_wait_list */,
                  const cl_event *  /* event_wait_list */,
                  cl_event *        /* event */,
                  cl_int *          /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueUnmapMemObject(cl_command_queue /* command_queue */,
                        cl_mem           /* memobj */,
                        void *           /* mapped_ptr */,
                        cl_uint          /* num_events_in_wait_list */,
                        const cl_event *  /* event_wait_list */,
                        cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueMigrateMemObjects(cl_command_queue       /* command_queue */,
                           cl_uint                /* num_mem_objects */,
                           const cl_mem *         /* mem_objects */,
                           cl_mem_migration_flags /* flags */,
                           cl_uint                /* num_events_in_wait_list */,
                           const cl_event *       /* event_wait_list */,
                           cl_event *             /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueNDRangeKernel(cl_command_queue /* command_queue */,
                       cl_kernel        /* kernel */,
                       cl_uint          /* work_dim */,
                       const size_t *   /* global_work_offset */,
                       const size_t *   /* global_work_size */,
                       const size_t *   /* local_work_size */,
                       cl_uint          /* num_events_in_wait_list */,
                       const cl_event * /* event_wait_list */,
                       cl_event *       /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueNativeKernel(cl_command_queue  /* command_queue */,
					  void (*user_func)(void *), 
                      void *            /* args */,
                      size_t            /* cb_args */, 
                      cl_uint           /* num_mem_objects */,
                      const cl_mem *    /* mem_list */,
                      const void **     /* args_mem_loc */,
                      cl_uint           /* num_events_in_wait_list */,
                      const cl_event *  /* event_wait_list */,
                      cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueMarkerWithWaitList(cl_command_queue  /* command_queue */,
                            cl_uint           /* num_events_in_wait_list */,
                            const cl_event *  /* event_wait_list */,
                            cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueBarrierWithWaitList(cl_command_queue  /* command_queue */,
                             cl_uint           /* num_events_in_wait_list */,
                             const cl_event *  /* event_wait_list */,
                             cl_event *        /* event */) CL_API_SUFFIX__VERSION_1_2;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMFree(cl_command_queue  /* command_queue */,
                 cl_uint           /* num_svm_pointers */,
                 void *[]          /* svm_pointers[] */,
                 void (CL_CALLBACK * /*pfn_free_func*/)(cl_command_queue /* queue */,
                                                        cl_uint          /* num_svm_pointers */,
                                                        void *[]         /* svm_pointers[] */,
                                                        void *           /* user_data */),
                 void *            /* user_data */,
                 cl_uint           /* num_events_in_wait_list */,
                 const cl_event *  /* event_wait_list */,
                 cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMemcpy(cl_command_queue  /* command_queue */,
                   cl_bool           /* blocking_copy */,
                   void *            /* dst_ptr */,
                   const void *      /* src_ptr */,
                   size_t            /* size */,
                   cl_uint           /* num_events_in_wait_list */,
                   const cl_event *  /* event_wait_list */,
                   cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMemFill(cl_command_queue  /* command_queue */,
                    void *            /* svm_ptr */,
                    const void *      /* pattern */,
                    size_t            /* pattern_size */,
                    size_t            /* size */,
                    cl_uint           /* num_events_in_wait_list */,
                    const cl_event *  /* event_wait_list */,
                    cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMMap(cl_command_queue  /* command_queue */,
                cl_bool           /* blocking_map */,
                cl_map_flags      /* flags */,
                void *            /* svm_ptr */,
                size_t            /* size */,
                cl_uint           /* num_events_in_wait_list */,
                const cl_event *  /* event_wait_list */,
                cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;
    
extern CL_API_ENTRY cl_int CL_API_CALL
clEnqueueSVMUnmap(cl_command_queue  /* command_queue */,
                  void *            /* svm_ptr */,
                  cl_uint           /* num_events_in_wait_list */,
                  const cl_event *  /* event_wait_list */,
                  cl_event *        /* event */) CL_API_SUFFIX__VERSION_2_0;

// Extension function access
//
// Returns the extension function address for the given function name,
// or NULL if a valid function can not be found.  The client must
// check to make sure the address is not NULL, before using or 
// calling the returned function address.
//
extern CL_API_ENTRY void * CL_API_CALL 
clGetExtensionFunctionAddressForPlatform(cl_platform_id /* platform */,
                                         const char *   /* func_name */) CL_API_SUFFIX__VERSION_1_2;

/* Deprecated OpenCL 1.1 APIs */
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_mem CL_API_CALL
clCreateImage2D(cl_context              /* context */,
                cl_mem_flags            /* flags */,
                const cl_image_format * /* image_format */,
                size_t                  /* image_width */,
                size_t                  /* image_height */,
                size_t                  /* image_row_pitch */, 
                void *                  /* host_ptr */,
                cl_int *                /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_mem CL_API_CALL
clCreateImage3D(cl_context              /* context */,
                cl_mem_flags            /* flags */,
                const cl_image_format * /* image_format */,
                size_t                  /* image_width */, 
                size_t                  /* image_height */,
                size_t                  /* image_depth */, 
                size_t                  /* image_row_pitch */, 
                size_t                  /* image_slice_pitch */, 
                void *                  /* host_ptr */,
                cl_int *                /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueMarker(cl_command_queue    /* command_queue */,
                cl_event *          /* event */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueWaitForEvents(cl_command_queue /* command_queue */,
                        cl_uint          /* num_events */,
                        const cl_event * /* event_list */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clEnqueueBarrier(cl_command_queue /* command_queue */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;

extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED cl_int CL_API_CALL
clUnloadCompiler(void) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_1_DEPRECATED void * CL_API_CALL
clGetExtensionFunctionAddress(const char * /* func_name */) CL_EXT_SUFFIX__VERSION_1_1_DEPRECATED;
    
/* Deprecated OpenCL 2.0 APIs */
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_command_queue CL_API_CALL
clCreateCommandQueue(cl_context                     /* context */,
                     cl_device_id                   /* device */,
                     cl_command_queue_properties    /* properties */,
                     cl_int *                       /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
    
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_sampler CL_API_CALL
clCreateSampler(cl_context          /* context */,
                cl_bool             /* normalized_coords */,
                cl_addressing_mode  /* addressing_mode */,
                cl_filter_mode      /* filter_mode */,
                cl_int *            /* errcode_ret */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
    
extern CL_API_ENTRY CL_EXT_PREFIX__VERSION_1_2_DEPRECATED cl_int CL_API_CALL
clEnqueueTask(cl_command_queue  /* command_queue */,
              cl_kernel         /* kernel */,
              cl_uint           /* num_events_in_wait_list */,
              const cl_event *  /* event_wait_list */,
              cl_event *        /* event */) CL_EXT_SUFFIX__VERSION_1_2_DEPRECATED;
'
! !

!OpenCLPrimitiveInterface class methodsFor: 'accessing' stamp: 'jmv 8/20/2015 16:04'!
current
	ActiveSubclass ifNil: [ ActiveSubclass _ self subclassForCurrentPlatform].
	^ActiveSubclass! !

!OpenCLPrimitiveInterface class methodsFor: 'finalization' stamp: 'jmv 8/20/2015 13:05'!
finalizationRegistry
	^FinalizationRegistry ifNil: [FinalizationRegistry := WeakRegistry new]! !

!OpenCLPrimitiveInterface class methodsFor: 'finalization' stamp: 'jmv 8/20/2015 13:05'!
finalizeValues
	self finalizationRegistry finalizeValues! !

!OpenCL64BitsPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 5/17/2017 12:27:52'!
generateFFICall: returnType name: funcName args: args cat: category

	"Compile."
	| publicMethod opaqueHandlerTypes answersHandle answerClass |
	"Not if not needed"
	opaqueHandlerTypes _ self opaqueHandlerTypes.
	answersHandle _ opaqueHandlerTypes includes: returnType.
	(answersHandle or: [
		(args anySatisfy: [ :arg | self opaqueHandlerTypes includes: arg key])]) ifFalse: [
			^ super generateFFICall: returnType name: funcName args: args cat: category ].
	
	"The method calling the external library is now private"
	self class
		compile: (self generateMethod: 'private_', funcName returnType: returnType name: funcName args: args)
		classified: ('FFI-generated-private-', category) 
		withStamp: nil
		notifying: nil
		logSource: true.

	"Add a public method"
	publicMethod _ String streamContents: [ :stream |
		"Write the method signature."
		self generateFFICallSignatureName: funcName args: args on: stream.
		stream newLine; newLine; tab; nextPutAll: '"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"'.
		stream newLine; newLine; tab; nextPutAll: '| answer |'.
		stream newLine; tab; nextPutAll: 'answer := self '.
		self generateFFIPrivateCallName: 'private_', funcName args: args on: stream.
		stream newLine.
		answersHandle ifTrue: [
			answerClass _ self typeMap at: returnType.
			stream newLine; tab; nextPutAll: 'answer := ', answerClass, ' new setHandle: answer getHandle asByteArrayPointer.'.
			].
		stream newLine; tab; nextPutAll: '^ answer'.
	].

	self class
		compile: publicMethod
		classified: ('FFI-generated-', category) 
		withStamp: nil
		notifying: nil
		logSource: true! !

!OpenCL64BitsPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 5/17/2017 12:27:55'!
generateFFIPrivateCallName: funcName args: args on: stream
	"Write the method signature.  Each argument after the first gets a keyword that tells about the C type."
	stream nextPutAll: funcName.
	(args size > 0) ifTrue: [stream nextPutAll: '__' ].
	args do: [ :arg |
		| argType argName |
		argType := self adaptType: arg key.
		stream nextPutAll: argType; nextPut: $:.
		argName := arg value.
		argName at: 1 put: argName first asLowercase.
		"Some are array-types (i.e 'origin[3]'), so strip off the end."
		argName := argName upTo: $[.
		"Write the name."
		stream space; nextPutAll: argName; space.
		"Add #getHandler call if needed"
		(self opaqueHandlerTypes includes: arg key) ifTrue: [
			stream nextPutAll: 'getHandle asExternalPointer'
			].
	] separatedBy: [
		stream newLineTab: 6.
	].
	stream nextPut: $.! !

!OpenCL64BitsPrimitiveInterface class methodsFor: 'code generation' stamp: 'jmv 5/17/2017 12:27:58'!
typeOf: inputTypeName
	(self opaqueHandlerTypes includes: inputTypeName) ifTrue: [
		^'void*'].
	^ super typeOf: inputTypeName! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clBuildProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCompileProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBinary__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBuiltInKernels__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithSource__cl_context: context getHandle asExternalPointer
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramBuildInfo__cl_program: program getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramInfo__cl_program: program getHandle asExternalPointer
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clLinkProgram__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clUnloadPlatformCompiler__cl_platform_id: platform getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateBuffer__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreatePipe__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubBuffer__cl_mem: buffer getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetImageInfo__cl_mem: image getHandle asExternalPointer
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetMemObjectInfo__cl_mem: memobj getHandle asExternalPointer
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPipeInfo__cl_mem: pipe getHandle asExternalPointer
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSupportedImageFormats__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetMemObjectDestructorCallback__cl_mem: memobj getHandle asExternalPointer
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueueWithProperties__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetCommandQueueInfo__cl_command_queue: command_queue getHandle asExternalPointer
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueue__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSampler__cl_context: context getHandle asExternalPointer
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueTask__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetContextInfo__cl_context: context getHandle asExternalPointer
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage2D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage3D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrier__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarker__cl_command_queue: command_queue getHandle asExternalPointer
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWaitForEvents__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events 
						const__cl_event__ptr: event_list .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernelsInProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernel__cl_program: program getHandle asExternalPointer
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret .

	answer := CLKernel new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelArgInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelWorkGroupInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArgSVMPointer__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArg__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelExecInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSamplerWithProperties__cl_context: context getHandle asExternalPointer
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSamplerInfo__cl_sampler: sampler getHandle asExternalPointer
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubDevices__cl_device_id: in_device getHandle asExternalPointer
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceIDs__cl_platform_id: platform getHandle asExternalPointer
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceInfo__cl_device_id: device getHandle asExternalPointer
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateUserEvent__cl_context: context getHandle asExternalPointer
						cl_int__ptr: errcode_ret .

	answer := CLEvent new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventInfo__cl_event: event getHandle asExternalPointer
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetEventCallback__cl_event: event getHandle asExternalPointer
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetUserEventStatus__cl_event: event getHandle asExternalPointer
						cl_int: execution_status .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNDRangeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNativeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMFree__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMap__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemFill__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemcpy__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMUnmap__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueUnmapMemObject__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: memobj getHandle asExternalPointer
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFinish__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFlush__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventProfilingInfo__cl_event: event getHandle asExternalPointer
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform getHandle asExternalPointer
						const__char__ptr: func_name .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPlatformInfo__cl_platform_id: platform getHandle asExternalPointer
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMAlloc__cl_context: context getHandle asExternalPointer
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMFree__cl_context: context getHandle asExternalPointer
						void__ptr: svm_pointer .

	^ answer! !

!OpenCLMacOSInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:03'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'OpenCL.framework'! !

!OpenCLMacOSInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:13'!
supportedPlatform
	^'Mac OS'! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (void* ulong CLDeviceID* schar* void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBinary' (void* ulong CLDeviceID* ulong* void* long* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBuiltInKernels' (void* ulong CLDeviceID* schar* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithSource' (void* ulong void* ulong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (void* void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clLinkProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateBuffer' (void* ulonglong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage' (void* ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreatePipe' (void* ulonglong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSubBuffer' (void* ulonglong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (void* ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (void* void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueueWithProperties' (void* void* ulonglong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueue' (void* void* ulonglong long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSampler' (void* ulong ulong ulong long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage2D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage3D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (void* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (void* ulong CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (void* ulong CLKernel* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateKernel' (void* schar* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (void* ulong ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (void* void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (void* ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (void* ulong ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (void* ulong ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSamplerWithProperties' (void* ulonglong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (void* void* ulong CLDeviceID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (void* ulonglong ulong CLDeviceID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateUserEvent' (void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (void* long void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (void* long) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (void* void* void* ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (void* void* void* ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (void* void* void* ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (void* void* void* ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (void* void* void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (void* void* ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (void* void* ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (void* ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (void* void* ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (void* void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (void* ulong void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (void* ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (void* void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (void* ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Profiling APIs'!
private_clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Extension function access'!
private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (void* schar*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-Platform API'!
private_clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (void* ulonglong ulong ulong) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOSInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clBuildProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCompileProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBinary__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBuiltInKernels__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithSource__cl_context: context getHandle asExternalPointer
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramBuildInfo__cl_program: program getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramInfo__cl_program: program getHandle asExternalPointer
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clLinkProgram__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clUnloadPlatformCompiler__cl_platform_id: platform getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateBuffer__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreatePipe__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubBuffer__cl_mem: buffer getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetImageInfo__cl_mem: image getHandle asExternalPointer
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetMemObjectInfo__cl_mem: memobj getHandle asExternalPointer
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPipeInfo__cl_mem: pipe getHandle asExternalPointer
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSupportedImageFormats__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetMemObjectDestructorCallback__cl_mem: memobj getHandle asExternalPointer
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueueWithProperties__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetCommandQueueInfo__cl_command_queue: command_queue getHandle asExternalPointer
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueue__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSampler__cl_context: context getHandle asExternalPointer
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueTask__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetContextInfo__cl_context: context getHandle asExternalPointer
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage2D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage3D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrier__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarker__cl_command_queue: command_queue getHandle asExternalPointer
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWaitForEvents__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events 
						const__cl_event__ptr: event_list .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernelsInProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernel__cl_program: program getHandle asExternalPointer
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret .

	answer := CLKernel new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelArgInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelWorkGroupInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArgSVMPointer__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArg__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelExecInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSamplerWithProperties__cl_context: context getHandle asExternalPointer
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSamplerInfo__cl_sampler: sampler getHandle asExternalPointer
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubDevices__cl_device_id: in_device getHandle asExternalPointer
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceIDs__cl_platform_id: platform getHandle asExternalPointer
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceInfo__cl_device_id: device getHandle asExternalPointer
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateUserEvent__cl_context: context getHandle asExternalPointer
						cl_int__ptr: errcode_ret .

	answer := CLEvent new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventInfo__cl_event: event getHandle asExternalPointer
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetEventCallback__cl_event: event getHandle asExternalPointer
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetUserEventStatus__cl_event: event getHandle asExternalPointer
						cl_int: execution_status .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNDRangeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNativeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMFree__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMap__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemFill__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemcpy__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMUnmap__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueUnmapMemObject__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: memobj getHandle asExternalPointer
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFinish__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFlush__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventProfilingInfo__cl_event: event getHandle asExternalPointer
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform getHandle asExternalPointer
						const__char__ptr: func_name .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPlatformInfo__cl_platform_id: platform getHandle asExternalPointer
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMAlloc__cl_context: context getHandle asExternalPointer
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMFree__cl_context: context getHandle asExternalPointer
						void__ptr: svm_pointer .

	^ answer! !

!OpenCLUnixInterface class methodsFor: 'class initialization' stamp: 'jmv 4/17/2017 15:10:03'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'libOpenCL.so.1'! !

!OpenCLUnixInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 15:45'!
supportedPlatform
	^'unix'! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (void* ulong CLDeviceID* schar* void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBinary' (void* ulong CLDeviceID* ulong* void* long* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBuiltInKernels' (void* ulong CLDeviceID* schar* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithSource' (void* ulong void* ulong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (void* void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clLinkProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateBuffer' (void* ulonglong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage' (void* ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreatePipe' (void* ulonglong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSubBuffer' (void* ulonglong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (void* ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (void* void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueueWithProperties' (void* void* ulonglong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueue' (void* void* ulonglong long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSampler' (void* ulong ulong ulong long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (void* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage2D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage3D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (void* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (void* ulong CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (void* ulong CLKernel* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateKernel' (void* schar* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (void* ulong ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (void* void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (void* ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (void* ulong ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (void* ulong ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSamplerWithProperties' (void* ulonglong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (void* void* ulong CLDeviceID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (void* ulonglong ulong CLDeviceID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateUserEvent' (void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (void* long void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (void* long) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (void* void* void* ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (void* void* void* ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (void* void* void* ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (void* void* void* ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (void* void* void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (void* void* ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (void* void* ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (void* ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (void* void* ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (void* void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (void* ulong void* void* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (void* ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (void* void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (void* ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (void* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (void* void* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Profiling APIs'!
private_clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Extension function access'!
private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (void* schar*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-Platform API'!
private_clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (void* ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (void* ulonglong ulong ulong) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnixInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clBuildProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCompileProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBinary__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithBuiltInKernels__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateProgramWithSource__cl_context: context getHandle asExternalPointer
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramBuildInfo__cl_program: program getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetProgramInfo__cl_program: program getHandle asExternalPointer
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clLinkProgram__cl_context: context getHandle asExternalPointer
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLProgram new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainProgram__cl_program: program getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clUnloadPlatformCompiler__cl_platform_id: platform getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateBuffer__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreatePipe__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubBuffer__cl_mem: buffer getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetImageInfo__cl_mem: image getHandle asExternalPointer
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetMemObjectInfo__cl_mem: memobj getHandle asExternalPointer
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPipeInfo__cl_mem: pipe getHandle asExternalPointer
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSupportedImageFormats__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainMemObject__cl_mem: memobj getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetMemObjectDestructorCallback__cl_mem: memobj getHandle asExternalPointer
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueueWithProperties__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetCommandQueueInfo__cl_command_queue: command_queue getHandle asExternalPointer
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainCommandQueue__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateCommandQueue__cl_context: context getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret .

	answer := CLCommandQueue new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSampler__cl_context: context getHandle asExternalPointer
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueTask__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret .

	answer := CLContext new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetContextInfo__cl_context: context getHandle asExternalPointer
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainContext__cl_context: context getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage2D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateImage3D__cl_context: context getHandle asExternalPointer
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret .

	answer := CLMem new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrier__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarker__cl_command_queue: command_queue getHandle asExternalPointer
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWaitForEvents__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events 
						const__cl_event__ptr: event_list .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernelsInProgram__cl_program: program getHandle asExternalPointer
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateKernel__cl_program: program getHandle asExternalPointer
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret .

	answer := CLKernel new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelArgInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetKernelWorkGroupInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_device_id: device getHandle asExternalPointer
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainKernel__cl_kernel: kernel getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArgSVMPointer__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelArg__cl_kernel: kernel getHandle asExternalPointer
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetKernelExecInfo__cl_kernel: kernel getHandle asExternalPointer
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSamplerWithProperties__cl_context: context getHandle asExternalPointer
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret .

	answer := CLSampler new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetSamplerInfo__cl_sampler: sampler getHandle asExternalPointer
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainSampler__cl_sampler: sampler getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateSubDevices__cl_device_id: in_device getHandle asExternalPointer
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceIDs__cl_platform_id: platform getHandle asExternalPointer
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetDeviceInfo__cl_device_id: device getHandle asExternalPointer
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainDevice__cl_device_id: device getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clCreateUserEvent__cl_context: context getHandle asExternalPointer
						cl_int__ptr: errcode_ret .

	answer := CLEvent new setHandle: answer getHandle asByteArrayPointer.
	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventInfo__cl_event: event getHandle asExternalPointer
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clReleaseEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clRetainEvent__cl_event: event getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetEventCallback__cl_event: event getHandle asExternalPointer
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSetUserEventStatus__cl_event: event getHandle asExternalPointer
						cl_int: execution_status .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_buffer getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_buffer getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueCopyImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: src_image getHandle asExternalPointer
						cl_mem: dst_image getHandle asExternalPointer
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueFillImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMapImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNDRangeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						cl_kernel: kernel getHandle asExternalPointer
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueNativeKernel__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueReadImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMFree__cl_command_queue: command_queue getHandle asExternalPointer
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMap__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemFill__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMMemcpy__cl_command_queue: command_queue getHandle asExternalPointer
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueSVMUnmap__cl_command_queue: command_queue getHandle asExternalPointer
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueUnmapMemObject__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: memobj getHandle asExternalPointer
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBufferRect__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteBuffer__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: buffer getHandle asExternalPointer
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clEnqueueWriteImage__cl_command_queue: command_queue getHandle asExternalPointer
						cl_mem: image getHandle asExternalPointer
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFinish__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clFlush__cl_command_queue: command_queue getHandle asExternalPointer.

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetEventProfilingInfo__cl_event: event getHandle asExternalPointer
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform getHandle asExternalPointer
						const__char__ptr: func_name .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clGetPlatformInfo__cl_platform_id: platform getHandle asExternalPointer
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMAlloc__cl_context: context getHandle asExternalPointer
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"Public method. Needed for calling #getHandle on OpenCLOpaqueHandle arguments"

	| answer |
	answer := self private_clSVMFree__cl_context: context getHandle asExternalPointer
						void__ptr: svm_pointer .

	^ answer! !

!OpenCLWindowsInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:04'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'OpenCL.dll'! !

!OpenCLWindowsInterface class methodsFor: 'class initialization' stamp: 'jmv 8/20/2015 13:14'!
supportedPlatform
	^'Win32'! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (void* ulong CLDeviceID* schar* void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBinary' (void* ulong CLDeviceID* ulong* void* long* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithBuiltInKernels' (void* ulong CLDeviceID* schar* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateProgramWithSource' (void* ulong void* ulong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (void* void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clLinkProgram' (void* ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Program Object APIs'!
private_clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateBuffer' (void* ulonglong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage' (void* ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreatePipe' (void* ulonglong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSubBuffer' (void* ulonglong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (void* ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Memory Object APIs'!
private_clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (void* void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueueWithProperties' (void* void* ulonglong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Command Queue APIs'!
private_clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateCommandQueue' (void* void* ulonglong long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSampler' (void* ulong ulong ulong long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 2.0 APIs'!
private_clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Context APIs'!
private_clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage2D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateImage3D' (void* ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (void* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Deprecated OpenCL 1.1 APIs'!
private_clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (void* ulong CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (void* ulong CLKernel* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateKernel' (void* schar* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (void* ulong ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (void* void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (void* ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (void* ulong ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Kernel Object APIs'!
private_clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (void* ulong ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateSamplerWithProperties' (void* ulonglong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Sampler APIs'!
private_clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (void* void* ulong CLDeviceID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (void* ulonglong ulong CLDeviceID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Device APIs'!
private_clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clCreateUserEvent' (void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (void* long void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Event Object APIs'!
private_clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (void* long) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (void* void* void* ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (void* void* void* ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (void* void* void* ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (void* void* void* ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (void* void* void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (void* void* void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (void* void* ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (void* void* ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (void* ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (void* void* ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (void* void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (void* ulong void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (void* ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (void* void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (void* ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (void* void* ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (void* void* ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Enqueued Commands APIs'!
private_clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (void* void* ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Flush and Finish APIs'!
private_clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Profiling APIs'!
private_clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Extension function access'!
private_clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (void* schar*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-Platform API'!
private_clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (void* ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (void* ulonglong ulong ulong) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindowsInterface class methodsFor: 'FFI-generated-private-SVM Allocation APIs'!
private_clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (CLProgram ulong CLDeviceID* schar* void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (CLProgram ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBinary' (CLContext ulong CLDeviceID* ulong* void* long* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBuiltInKernels' (CLContext ulong CLDeviceID* schar* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithSource' (CLContext ulong void* ulong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (CLProgram CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (CLProgram ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clLinkProgram' (CLContext ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (CLProgram) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (CLProgram) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (CLPlatformID) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateBuffer' (CLContext ulonglong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage' (CLContext ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreatePipe' (CLContext ulonglong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateSubBuffer' (CLMem ulonglong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (CLContext ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (CLMem) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (CLMem) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (CLMem void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueueWithProperties' (CLContext CLDeviceID ulonglong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (CLCommandQueue ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (CLCommandQueue) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (CLCommandQueue) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueue' (CLContext CLDeviceID ulonglong long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSampler' (CLContext ulong ulong ulong long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (CLCommandQueue CLKernel ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (CLContext ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (CLContext) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (CLContext) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage2D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage3D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (CLCommandQueue) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (CLCommandQueue CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (CLCommandQueue ulong CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (CLProgram ulong CLKernel* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: CLKernel 'clCreateKernel' (CLProgram schar* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (CLKernel ulong ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (CLKernel ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (CLKernel CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (CLKernel) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (CLKernel) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (CLKernel ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (CLKernel ulong ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (CLKernel ulong ulong void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSamplerWithProperties' (CLContext ulonglong* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (CLSampler ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (CLSampler) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (CLSampler) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (CLDeviceID void* ulong CLDeviceID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (CLPlatformID ulonglong ulong CLDeviceID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (CLDeviceID) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (CLDeviceID) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: CLEvent 'clCreateUserEvent' (CLContext long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (CLEvent ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (CLEvent) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (CLEvent) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (CLEvent long void* void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (CLEvent long) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (CLCommandQueue CLMem CLMem ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (CLCommandQueue CLMem CLMem ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (CLCommandQueue CLMem CLMem ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (CLCommandQueue CLMem void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (CLCommandQueue CLMem void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (CLCommandQueue CLMem ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (CLCommandQueue CLMem ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (CLCommandQueue ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (CLCommandQueue CLKernel ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (CLCommandQueue void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (CLCommandQueue ulong void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (CLCommandQueue ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (CLCommandQueue void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (CLCommandQueue ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (CLCommandQueue void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (CLCommandQueue CLMem void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (CLCommandQueue) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (CLCommandQueue) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (CLEvent ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (CLPlatformID schar*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (CLPlatformID ulong ulong void* ulong*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (CLContext ulonglong ulong ulong) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (CLContext void*) module: 'OpenCL.framework'>

	^self externalCallFailed! !

!OpenCLMacOS32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 5/17/2017 12:25:59'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'OpenCL.framework'! !

!OpenCLMacOS32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 5/17/2017 12:25:59'!
supportedPlatform
	^'Mac OS'! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (CLProgram ulong CLDeviceID* schar* void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (CLProgram ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBinary' (CLContext ulong CLDeviceID* ulong* void* long* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBuiltInKernels' (CLContext ulong CLDeviceID* schar* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithSource' (CLContext ulong void* ulong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (CLProgram CLDeviceID ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (CLProgram ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clLinkProgram' (CLContext ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (CLProgram) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (CLProgram) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (CLPlatformID) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateBuffer' (CLContext ulonglong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage' (CLContext ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreatePipe' (CLContext ulonglong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateSubBuffer' (CLMem ulonglong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (CLMem ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (CLMem ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (CLMem ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (CLContext ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (CLMem) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (CLMem) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (CLMem void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueueWithProperties' (CLContext CLDeviceID ulonglong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (CLCommandQueue ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (CLCommandQueue) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (CLCommandQueue) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueue' (CLContext CLDeviceID ulonglong long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSampler' (CLContext ulong ulong ulong long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (CLCommandQueue CLKernel ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (CLContext ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (CLContext) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (CLContext) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage2D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage3D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (CLCommandQueue) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (CLCommandQueue CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (CLCommandQueue ulong CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (CLProgram ulong CLKernel* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: CLKernel 'clCreateKernel' (CLProgram schar* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (CLKernel ulong ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (CLKernel ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (CLKernel CLDeviceID ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (CLKernel) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (CLKernel) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (CLKernel ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (CLKernel ulong ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (CLKernel ulong ulong void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSamplerWithProperties' (CLContext ulonglong* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (CLSampler ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (CLSampler) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (CLSampler) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (CLDeviceID void* ulong CLDeviceID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (CLPlatformID ulonglong ulong CLDeviceID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (CLDeviceID ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (CLDeviceID) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (CLDeviceID) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: CLEvent 'clCreateUserEvent' (CLContext long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (CLEvent ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (CLEvent) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (CLEvent) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (CLEvent long void* void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (CLEvent long) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (CLCommandQueue CLMem CLMem ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (CLCommandQueue CLMem CLMem ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (CLCommandQueue CLMem CLMem ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (CLCommandQueue CLMem void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (CLCommandQueue CLMem void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (CLCommandQueue CLMem ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (CLCommandQueue CLMem ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (CLCommandQueue ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (CLCommandQueue CLKernel ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (CLCommandQueue void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (CLCommandQueue ulong void* void* void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (CLCommandQueue ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (CLCommandQueue void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (CLCommandQueue ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (CLCommandQueue void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (CLCommandQueue CLMem void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (CLCommandQueue) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (CLCommandQueue) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (CLEvent ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (CLPlatformID schar*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (CLPlatformID ulong ulong void* ulong*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (CLContext ulonglong ulong ulong) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (CLContext void*) module: 'libOpenCL.so.1'>

	^self externalCallFailed! !

!OpenCLUnix32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 4/17/2017 15:10:07'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'libOpenCL.so.1'! !

!OpenCLUnix32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 12/23/2016 11:36:10'!
supportedPlatform
	^'unix'! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clBuildProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						void__ptrpfn_notifycl_program: pfn_notify 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clBuildProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			void (*pfn_notify)(cl_program pfn_notify, 
			void * user_data
	);"
	<apicall: long 'clBuildProgram' (CLProgram ulong CLDeviceID* schar* void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCompileProgram__cl_program: program 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_headers 
						const__cl_program__ptr: input_headers 
						const__char__ptr__ptr: header_include_names 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clCompileProgram(
			cl_program program, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_headers, 
			const cl_program * input_headers, 
			const char ** header_include_names, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clCompileProgram' (CLProgram ulong CLDeviceID* schar* ulong CLProgram* void* void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBinary__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__size_t__ptr: lengths 
						const__unsignedchar__ptr__ptr: binaries 
						cl_int__ptr: binary_status 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBinary(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const size_t * lengths, 
			const unsigned char ** binaries, 
			cl_int * binary_status, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBinary' (CLContext ulong CLDeviceID* ulong* void* long* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithBuiltInKernels__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: kernel_names 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithBuiltInKernels(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * kernel_names, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithBuiltInKernels' (CLContext ulong CLDeviceID* schar* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clCreateProgramWithSource__cl_context: context 
						cl_uint: count 
						const__char__ptr__ptr: strings 
						const__size_t__ptr: lengths 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clCreateProgramWithSource(
			cl_context context, 
			cl_uint count, 
			const char ** strings, 
			const size_t * lengths, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clCreateProgramWithSource' (CLContext ulong void* ulong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramBuildInfo__cl_program: program 
						cl_device_id: device 
						cl_program_build_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramBuildInfo(
			cl_program program, 
			cl_device_id device, 
			cl_program_build_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramBuildInfo' (CLProgram CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clGetProgramInfo__cl_program: program 
						cl_program_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetProgramInfo(
			cl_program program, 
			cl_program_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetProgramInfo' (CLProgram ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clLinkProgram__cl_context: context 
						cl_uint: num_devices 
						const__cl_device_id__ptr: device_list 
						const__char__ptr: options 
						cl_uint: num_input_programs 
						const__cl_program__ptr: input_programs 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_program clLinkProgram(
			cl_context context, 
			cl_uint num_devices, 
			const cl_device_id * device_list, 
			const char * options, 
			cl_uint num_input_programs, 
			const cl_program * input_programs, 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLProgram 'clLinkProgram' (CLContext ulong CLDeviceID* schar* ulong CLProgram* void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clReleaseProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clReleaseProgram(
			cl_program program
	);"
	<apicall: long 'clReleaseProgram' (CLProgram) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clRetainProgram__cl_program: program 

	"This method was automatically generated."

	"cl_int clRetainProgram(
			cl_program program
	);"
	<apicall: long 'clRetainProgram' (CLProgram) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Program Object APIs'!
clUnloadPlatformCompiler__cl_platform_id: platform 

	"This method was automatically generated."

	"cl_int clUnloadPlatformCompiler(
			cl_platform_id platform
	);"
	<apicall: long 'clUnloadPlatformCompiler' (CLPlatformID) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateBuffer__cl_context: context 
						cl_mem_flags: flags 
						size_t: size 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateBuffer(
			cl_context context, 
			cl_mem_flags flags, 
			size_t size, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateBuffer' (CLContext ulonglong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateImage__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						const__cl_image_desc__ptr: image_desc 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			const cl_image_desc * image_desc, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage' (CLContext ulonglong CLImageFormatStruct* CLImageDescStruct* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreatePipe__cl_context: context 
						cl_mem_flags: flags 
						cl_uint: pipe_packet_size 
						cl_uint: pipe_max_packets 
						const__cl_pipe_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreatePipe(
			cl_context context, 
			cl_mem_flags flags, 
			cl_uint pipe_packet_size, 
			cl_uint pipe_max_packets, 
			const cl_pipe_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreatePipe' (CLContext ulonglong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clCreateSubBuffer__cl_mem: buffer 
						cl_mem_flags: flags 
						cl_buffer_create_type: buffer_create_type 
						const__void__ptr: buffer_create_info 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateSubBuffer(
			cl_mem buffer, 
			cl_mem_flags flags, 
			cl_buffer_create_type buffer_create_type, 
			const void * buffer_create_info, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateSubBuffer' (CLMem ulonglong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetImageInfo__cl_mem: image 
						cl_image_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetImageInfo(
			cl_mem image, 
			cl_image_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetImageInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetMemObjectInfo__cl_mem: memobj 
						cl_mem_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetMemObjectInfo(
			cl_mem memobj, 
			cl_mem_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetMemObjectInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetPipeInfo__cl_mem: pipe 
						cl_pipe_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPipeInfo(
			cl_mem pipe, 
			cl_pipe_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPipeInfo' (CLMem ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clGetSupportedImageFormats__cl_context: context 
						cl_mem_flags: flags 
						cl_mem_object_type: image_type 
						cl_uint: num_entries 
						cl_image_format__ptr: image_formats 
						cl_uint__ptr: num_image_formats 

	"This method was automatically generated."

	"cl_int clGetSupportedImageFormats(
			cl_context context, 
			cl_mem_flags flags, 
			cl_mem_object_type image_type, 
			cl_uint num_entries, 
			cl_image_format * image_formats, 
			cl_uint * num_image_formats
	);"
	<apicall: long 'clGetSupportedImageFormats' (CLContext ulonglong ulong ulong CLImageFormatStruct* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clReleaseMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clReleaseMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clReleaseMemObject' (CLMem) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clRetainMemObject__cl_mem: memobj 

	"This method was automatically generated."

	"cl_int clRetainMemObject(
			cl_mem memobj
	);"
	<apicall: long 'clRetainMemObject' (CLMem) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Memory Object APIs'!
clSetMemObjectDestructorCallback__cl_mem: memobj 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetMemObjectDestructorCallback(
			cl_mem memobj, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetMemObjectDestructorCallback' (CLMem void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clCreateCommandQueueWithProperties__cl_context: context 
						cl_device_id: device 
						const__cl_queue_properties__ptr: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueueWithProperties(
			cl_context context, 
			cl_device_id device, 
			const cl_queue_properties * properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueueWithProperties' (CLContext CLDeviceID ulonglong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clGetCommandQueueInfo__cl_command_queue: command_queue 
						cl_command_queue_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetCommandQueueInfo(
			cl_command_queue command_queue, 
			cl_command_queue_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetCommandQueueInfo' (CLCommandQueue ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clReleaseCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clReleaseCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clReleaseCommandQueue' (CLCommandQueue) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Command Queue APIs'!
clRetainCommandQueue__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clRetainCommandQueue(
			cl_command_queue command_queue
	);"
	<apicall: long 'clRetainCommandQueue' (CLCommandQueue) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateCommandQueue__cl_context: context 
						cl_device_id: device 
						cl_command_queue_properties: properties 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_command_queue clCreateCommandQueue(
			cl_context context, 
			cl_device_id device, 
			cl_command_queue_properties properties, 
			cl_int * errcode_ret
	);"
	<apicall: CLCommandQueue 'clCreateCommandQueue' (CLContext CLDeviceID ulonglong long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clCreateSampler__cl_context: context 
						cl_bool: normalized_coords 
						cl_addressing_mode: addressing_mode 
						cl_filter_mode: filter_mode 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSampler(
			cl_context context, 
			cl_bool normalized_coords, 
			cl_addressing_mode addressing_mode, 
			cl_filter_mode filter_mode, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSampler' (CLContext ulong ulong ulong long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 2.0 APIs'!
clEnqueueTask__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueTask(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueTask' (CLCommandQueue CLKernel ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContextFromType__const__cl_context_properties__ptr: properties 
						cl_device_type: device_type 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContextFromType(
			const cl_context_properties * properties, 
			cl_device_type device_type, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContextFromType' (void* ulonglong void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clCreateContext__const__cl_context_properties__ptr: properties 
						cl_uint: num_devices 
						const__cl_device_id__ptr: devices 
						void__ptrpfn_notifyconst__char__ptrconst__void__ptrsize_tvoid__ptr: pfn_notify 
						void__ptr: user_data 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_context clCreateContext(
			const cl_context_properties * properties, 
			cl_uint num_devices, 
			const cl_device_id * devices, 
			void (*pfn_notify)(const char *, const void *, size_t, void *) pfn_notify, 
			void * user_data, 
			cl_int * errcode_ret
	);"
	<apicall: CLContext 'clCreateContext' (void* ulong CLDeviceID* void* void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clGetContextInfo__cl_context: context 
						cl_context_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetContextInfo(
			cl_context context, 
			cl_context_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetContextInfo' (CLContext ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clReleaseContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clReleaseContext(
			cl_context context
	);"
	<apicall: long 'clReleaseContext' (CLContext) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Context APIs'!
clRetainContext__cl_context: context 

	"This method was automatically generated."

	"cl_int clRetainContext(
			cl_context context
	);"
	<apicall: long 'clRetainContext' (CLContext) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage2D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_row_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage2D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_row_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage2D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clCreateImage3D__cl_context: context 
						cl_mem_flags: flags 
						const__cl_image_format__ptr: image_format 
						size_t: image_width 
						size_t: image_height 
						size_t: image_depth 
						size_t: image_row_pitch 
						size_t: image_slice_pitch 
						void__ptr: host_ptr 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_mem clCreateImage3D(
			cl_context context, 
			cl_mem_flags flags, 
			const cl_image_format * image_format, 
			size_t image_width, 
			size_t image_height, 
			size_t image_depth, 
			size_t image_row_pitch, 
			size_t image_slice_pitch, 
			void * host_ptr, 
			cl_int * errcode_ret
	);"
	<apicall: CLMem 'clCreateImage3D' (CLContext ulonglong CLImageFormatStruct* ulong ulong ulong ulong ulong void* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueBarrier__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clEnqueueBarrier(
			cl_command_queue command_queue
	);"
	<apicall: long 'clEnqueueBarrier' (CLCommandQueue) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueMarker__cl_command_queue: command_queue 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarker(
			cl_command_queue command_queue, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarker' (CLCommandQueue CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clEnqueueWaitForEvents__cl_command_queue: command_queue 
						cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clEnqueueWaitForEvents(
			cl_command_queue command_queue, 
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clEnqueueWaitForEvents' (CLCommandQueue ulong CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clGetExtensionFunctionAddress__const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddress(
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddress' (schar*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Deprecated OpenCL 1.1 APIs'!
clUnloadCompiler

	"This method was automatically generated."

	"cl_int clUnloadCompiler(
	);"
	<apicall: long 'clUnloadCompiler' () module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernelsInProgram__cl_program: program 
						cl_uint: num_kernels 
						cl_kernel__ptr: kernels 
						cl_uint__ptr: num_kernels_ret 

	"This method was automatically generated."

	"cl_int clCreateKernelsInProgram(
			cl_program program, 
			cl_uint num_kernels, 
			cl_kernel * kernels, 
			cl_uint * num_kernels_ret
	);"
	<apicall: long 'clCreateKernelsInProgram' (CLProgram ulong CLKernel* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clCreateKernel__cl_program: program 
						const__char__ptr: kernel_name 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_kernel clCreateKernel(
			cl_program program, 
			const char * kernel_name, 
			cl_int * errcode_ret
	);"
	<apicall: CLKernel 'clCreateKernel' (CLProgram schar* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelArgInfo__cl_kernel: kernel 
						cl_uint: arg_indx 
						cl_kernel_arg_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelArgInfo(
			cl_kernel kernel, 
			cl_uint arg_indx, 
			cl_kernel_arg_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelArgInfo' (CLKernel ulong ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelInfo__cl_kernel: kernel 
						cl_kernel_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelInfo(
			cl_kernel kernel, 
			cl_kernel_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelInfo' (CLKernel ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clGetKernelWorkGroupInfo__cl_kernel: kernel 
						cl_device_id: device 
						cl_kernel_work_group_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetKernelWorkGroupInfo(
			cl_kernel kernel, 
			cl_device_id device, 
			cl_kernel_work_group_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetKernelWorkGroupInfo' (CLKernel CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clReleaseKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clReleaseKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clReleaseKernel' (CLKernel) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clRetainKernel__cl_kernel: kernel 

	"This method was automatically generated."

	"cl_int clRetainKernel(
			cl_kernel kernel
	);"
	<apicall: long 'clRetainKernel' (CLKernel) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArgSVMPointer__cl_kernel: kernel 
						cl_uint: arg_index 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArgSVMPointer(
			cl_kernel kernel, 
			cl_uint arg_index, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArgSVMPointer' (CLKernel ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelArg__cl_kernel: kernel 
						cl_uint: arg_index 
						size_t: arg_size 
						const__void__ptr: arg_value 

	"This method was automatically generated."

	"cl_int clSetKernelArg(
			cl_kernel kernel, 
			cl_uint arg_index, 
			size_t arg_size, 
			const void * arg_value
	);"
	<apicall: long 'clSetKernelArg' (CLKernel ulong ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Kernel Object APIs'!
clSetKernelExecInfo__cl_kernel: kernel 
						cl_kernel_exec_info: param_name 
						size_t: param_value_size 
						const__void__ptr: param_value 

	"This method was automatically generated."

	"cl_int clSetKernelExecInfo(
			cl_kernel kernel, 
			cl_kernel_exec_info param_name, 
			size_t param_value_size, 
			const void * param_value
	);"
	<apicall: long 'clSetKernelExecInfo' (CLKernel ulong ulong void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clCreateSamplerWithProperties__cl_context: context 
						const__cl_sampler_properties__ptr: normalized_coords 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_sampler clCreateSamplerWithProperties(
			cl_context context, 
			const cl_sampler_properties * normalized_coords, 
			cl_int * errcode_ret
	);"
	<apicall: CLSampler 'clCreateSamplerWithProperties' (CLContext ulonglong* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clGetSamplerInfo__cl_sampler: sampler 
						cl_sampler_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetSamplerInfo(
			cl_sampler sampler, 
			cl_sampler_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetSamplerInfo' (CLSampler ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clReleaseSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clReleaseSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clReleaseSampler' (CLSampler) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Sampler APIs'!
clRetainSampler__cl_sampler: sampler 

	"This method was automatically generated."

	"cl_int clRetainSampler(
			cl_sampler sampler
	);"
	<apicall: long 'clRetainSampler' (CLSampler) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clCreateSubDevices__cl_device_id: in_device 
						const__cl_device_partition_property__ptr: properties 
						cl_uint: num_devices 
						cl_device_id__ptr: out_devices 
						cl_uint__ptr: num_devices_ret 

	"This method was automatically generated."

	"cl_int clCreateSubDevices(
			cl_device_id in_device, 
			const cl_device_partition_property * properties, 
			cl_uint num_devices, 
			cl_device_id * out_devices, 
			cl_uint * num_devices_ret
	);"
	<apicall: long 'clCreateSubDevices' (CLDeviceID void* ulong CLDeviceID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceIDs__cl_platform_id: platform 
						cl_device_type: device_type 
						cl_uint: num_entries 
						cl_device_id__ptr: devices 
						cl_uint__ptr: num_devices 

	"This method was automatically generated."

	"cl_int clGetDeviceIDs(
			cl_platform_id platform, 
			cl_device_type device_type, 
			cl_uint num_entries, 
			cl_device_id * devices, 
			cl_uint * num_devices
	);"
	<apicall: long 'clGetDeviceIDs' (CLPlatformID ulonglong ulong CLDeviceID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clGetDeviceInfo__cl_device_id: device 
						cl_device_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetDeviceInfo(
			cl_device_id device, 
			cl_device_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetDeviceInfo' (CLDeviceID ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clReleaseDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clReleaseDevice(
			cl_device_id device
	);"
	<apicall: long 'clReleaseDevice' (CLDeviceID) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Device APIs'!
clRetainDevice__cl_device_id: device 

	"This method was automatically generated."

	"cl_int clRetainDevice(
			cl_device_id device
	);"
	<apicall: long 'clRetainDevice' (CLDeviceID) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clCreateUserEvent__cl_context: context 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"cl_event clCreateUserEvent(
			cl_context context, 
			cl_int * errcode_ret
	);"
	<apicall: CLEvent 'clCreateUserEvent' (CLContext long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clGetEventInfo__cl_event: event 
						cl_event_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventInfo(
			cl_event event, 
			cl_event_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventInfo' (CLEvent ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clReleaseEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clReleaseEvent(
			cl_event event
	);"
	<apicall: long 'clReleaseEvent' (CLEvent) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clRetainEvent__cl_event: event 

	"This method was automatically generated."

	"cl_int clRetainEvent(
			cl_event event
	);"
	<apicall: long 'clRetainEvent' (CLEvent) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetEventCallback__cl_event: event 
						cl_int: command_exec_callback_type 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 

	"This method was automatically generated."

	"cl_int clSetEventCallback(
			cl_event event, 
			cl_int command_exec_callback_type, 
			void * no_callbacks_jmv, 
			void * user_data
	);"
	<apicall: long 'clSetEventCallback' (CLEvent long void* void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clSetUserEventStatus__cl_event: event 
						cl_int: execution_status 

	"This method was automatically generated."

	"cl_int clSetUserEventStatus(
			cl_event event, 
			cl_int execution_status
	);"
	<apicall: long 'clSetUserEventStatus' (CLEvent long) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Event Object APIs'!
clWaitForEvents__cl_uint: num_events 
						const__cl_event__ptr: event_list 

	"This method was automatically generated."

	"cl_int clWaitForEvents(
			cl_uint num_events, 
			const cl_event * event_list
	);"
	<apicall: long 'clWaitForEvents' (ulong CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueBarrierWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueBarrierWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueBarrierWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferRect__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						size_t: src_row_pitch 
						size_t: src_slice_pitch 
						size_t: dst_row_pitch 
						size_t: dst_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferRect(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			const size_t * src_origin, 
			const size_t * dst_origin, 
			const size_t * region, 
			size_t src_row_pitch, 
			size_t src_slice_pitch, 
			size_t dst_row_pitch, 
			size_t dst_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferRect' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBufferToImage__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_image 
						size_t: src_offset 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBufferToImage(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_image, 
			size_t src_offset, 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBufferToImage' (CLCommandQueue CLMem CLMem ulong ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyBuffer__cl_command_queue: command_queue 
						cl_mem: src_buffer 
						cl_mem: dst_buffer 
						size_t: src_offset 
						size_t: dst_offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyBuffer(
			cl_command_queue command_queue, 
			cl_mem src_buffer, 
			cl_mem dst_buffer, 
			size_t src_offset, 
			size_t dst_offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyBuffer' (CLCommandQueue CLMem CLMem ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImageToBuffer__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_buffer 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: region 
						size_t: dst_offset 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImageToBuffer(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_buffer, 
			const size_t * src_origin[3], 
			const size_t * region[3], 
			size_t dst_offset, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImageToBuffer' (CLCommandQueue CLMem CLMem ulong* ulong* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueCopyImage__cl_command_queue: command_queue 
						cl_mem: src_image 
						cl_mem: dst_image 
						const__size_t__ptr: src_origin 
						const__size_t__ptr: dst_origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueCopyImage(
			cl_command_queue command_queue, 
			cl_mem src_image, 
			cl_mem dst_image, 
			const size_t * src_origin[3], 
			const size_t * dst_origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueCopyImage' (CLCommandQueue CLMem CLMem ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			const void * pattern, 
			size_t pattern_size, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillBuffer' (CLCommandQueue CLMem void* ulong ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueFillImage__cl_command_queue: command_queue 
						cl_mem: image 
						const__void__ptr: fill_color 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueFillImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			const void * fill_color, 
			const size_t * origin[3], 
			const size_t * region[3], 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueFillImage' (CLCommandQueue CLMem void* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						size_t: offset 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			size_t offset, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapBuffer' (CLCommandQueue CLMem ulong ulonglong ulong ulong ulong CLEvent* CLEvent* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMapImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_map 
						cl_map_flags: map_flags 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t__ptr: image_row_pitch 
						size_t__ptr: image_slice_pitch 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 
						cl_int__ptr: errcode_ret 

	"This method was automatically generated."

	"void * clEnqueueMapImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_map, 
			cl_map_flags map_flags, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t * image_row_pitch, 
			size_t * image_slice_pitch, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event, 
			cl_int * errcode_ret
	);"
	<apicall: void* 'clEnqueueMapImage' (CLCommandQueue CLMem ulong ulonglong ulong* ulong* ulong* ulong* ulong CLEvent* CLEvent* long*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMarkerWithWaitList__cl_command_queue: command_queue 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMarkerWithWaitList(
			cl_command_queue command_queue, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMarkerWithWaitList' (CLCommandQueue ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueMigrateMemObjects__cl_command_queue: command_queue 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_objects 
						cl_mem_migration_flags: flags 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueMigrateMemObjects(
			cl_command_queue command_queue, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_objects, 
			cl_mem_migration_flags flags, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueMigrateMemObjects' (CLCommandQueue ulong CLMem* ulonglong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNDRangeKernel__cl_command_queue: command_queue 
						cl_kernel: kernel 
						cl_uint: work_dim 
						const__size_t__ptr: global_work_offset 
						const__size_t__ptr: global_work_size 
						const__size_t__ptr: local_work_size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNDRangeKernel(
			cl_command_queue command_queue, 
			cl_kernel kernel, 
			cl_uint work_dim, 
			const size_t * global_work_offset, 
			const size_t * global_work_size, 
			const size_t * local_work_size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNDRangeKernel' (CLCommandQueue CLKernel ulong ulong* ulong* ulong* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueNativeKernel__cl_command_queue: command_queue 
						void__ptruser_funcvoid__ptrvoid__ptr: user_func 
						size_t: cb_args 
						cl_uint: num_mem_objects 
						const__cl_mem__ptr: mem_list 
						const__void__ptr__ptr: args_mem_loc 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueNativeKernel(
			cl_command_queue command_queue, 
			void (*user_func)(void *), 
                      void * user_func, 
			size_t cb_args, 
			cl_uint num_mem_objects, 
			const cl_mem * mem_list, 
			const void ** args_mem_loc, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueNativeKernel' (CLCommandQueue void* ulong ulong CLMem* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_read 
						size_t: offset 
						size_t: size 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_read, 
			size_t offset, 
			size_t size, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueReadImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_read 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: row_pitch 
						size_t: slice_pitch 
						void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueReadImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_read, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t row_pitch, 
			size_t slice_pitch, 
			void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueReadImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMFree__cl_command_queue: command_queue 
						cl_uint: num_svm_pointers 
						void__ptr: svm_pointers 
						void__ptr: no_callbacks_jmv 
						void__ptr: user_data 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMFree(
			cl_command_queue command_queue, 
			cl_uint num_svm_pointers, 
			void * svm_pointers[], 
			void * no_callbacks_jmv, 
			void * user_data, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMFree' (CLCommandQueue ulong void* void* void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMap__cl_command_queue: command_queue 
						cl_bool: blocking_map 
						cl_map_flags: flags 
						void__ptr: svm_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMap(
			cl_command_queue command_queue, 
			cl_bool blocking_map, 
			cl_map_flags flags, 
			void * svm_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMap' (CLCommandQueue ulong ulonglong void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemFill__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						const__void__ptr: pattern 
						size_t: pattern_size 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemFill(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			const void * pattern, 
			size_t pattern_size, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemFill' (CLCommandQueue void* void* ulong ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMMemcpy__cl_command_queue: command_queue 
						cl_bool: blocking_copy 
						void__ptr: dst_ptr 
						const__void__ptr: src_ptr 
						size_t: size 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMMemcpy(
			cl_command_queue command_queue, 
			cl_bool blocking_copy, 
			void * dst_ptr, 
			const void * src_ptr, 
			size_t size, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMMemcpy' (CLCommandQueue ulong void* void* ulong ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueSVMUnmap__cl_command_queue: command_queue 
						void__ptr: svm_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueSVMUnmap(
			cl_command_queue command_queue, 
			void * svm_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueSVMUnmap' (CLCommandQueue void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueUnmapMemObject__cl_command_queue: command_queue 
						cl_mem: memobj 
						void__ptr: mapped_ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueUnmapMemObject(
			cl_command_queue command_queue, 
			cl_mem memobj, 
			void * mapped_ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueUnmapMemObject' (CLCommandQueue CLMem void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBufferRect__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						const__size_t__ptr: buffer_offset 
						const__size_t__ptr: host_offset 
						const__size_t__ptr: region 
						size_t: buffer_row_pitch 
						size_t: buffer_slice_pitch 
						size_t: host_row_pitch 
						size_t: host_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBufferRect(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			const size_t * buffer_offset, 
			const size_t * host_offset, 
			const size_t * region, 
			size_t buffer_row_pitch, 
			size_t buffer_slice_pitch, 
			size_t host_row_pitch, 
			size_t host_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBufferRect' (CLCommandQueue CLMem ulong ulong* ulong* ulong* ulong ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteBuffer__cl_command_queue: command_queue 
						cl_mem: buffer 
						cl_bool: blocking_write 
						size_t: offset 
						size_t: size 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteBuffer(
			cl_command_queue command_queue, 
			cl_mem buffer, 
			cl_bool blocking_write, 
			size_t offset, 
			size_t size, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteBuffer' (CLCommandQueue CLMem ulong ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Enqueued Commands APIs'!
clEnqueueWriteImage__cl_command_queue: command_queue 
						cl_mem: image 
						cl_bool: blocking_write 
						const__size_t__ptr: origin 
						const__size_t__ptr: region 
						size_t: input_row_pitch 
						size_t: input_slice_pitch 
						const__void__ptr: ptr 
						cl_uint: num_events_in_wait_list 
						const__cl_event__ptr: event_wait_list 
						cl_event__ptr: event 

	"This method was automatically generated."

	"cl_int clEnqueueWriteImage(
			cl_command_queue command_queue, 
			cl_mem image, 
			cl_bool blocking_write, 
			const size_t * origin[3], 
			const size_t * region[3], 
			size_t input_row_pitch, 
			size_t input_slice_pitch, 
			const void * ptr, 
			cl_uint num_events_in_wait_list, 
			const cl_event * event_wait_list, 
			cl_event * event
	);"
	<apicall: long 'clEnqueueWriteImage' (CLCommandQueue CLMem ulong ulong* ulong* ulong ulong void* ulong CLEvent* CLEvent*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFinish__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFinish(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFinish' (CLCommandQueue) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Flush and Finish APIs'!
clFlush__cl_command_queue: command_queue 

	"This method was automatically generated."

	"cl_int clFlush(
			cl_command_queue command_queue
	);"
	<apicall: long 'clFlush' (CLCommandQueue) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Profiling APIs'!
clGetEventProfilingInfo__cl_event: event 
						cl_profiling_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetEventProfilingInfo(
			cl_event event, 
			cl_profiling_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetEventProfilingInfo' (CLEvent ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Extension function access'!
clGetExtensionFunctionAddressForPlatform__cl_platform_id: platform 
						const__char__ptr: func_name 

	"This method was automatically generated."

	"void * clGetExtensionFunctionAddressForPlatform(
			cl_platform_id platform, 
			const char * func_name
	);"
	<apicall: void* 'clGetExtensionFunctionAddressForPlatform' (CLPlatformID schar*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformIDs__cl_uint: num_entries 
						cl_platform_id__ptr: platforms 
						cl_uint__ptr: num_platforms 

	"This method was automatically generated."

	"cl_int clGetPlatformIDs(
			cl_uint num_entries, 
			cl_platform_id * platforms, 
			cl_uint * num_platforms
	);"
	<apicall: long 'clGetPlatformIDs' (ulong CLPlatformID* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-Platform API'!
clGetPlatformInfo__cl_platform_id: platform 
						cl_platform_info: param_name 
						size_t: param_value_size 
						void__ptr: param_value 
						size_t__ptr: param_value_size_ret 

	"This method was automatically generated."

	"cl_int clGetPlatformInfo(
			cl_platform_id platform, 
			cl_platform_info param_name, 
			size_t param_value_size, 
			void * param_value, 
			size_t * param_value_size_ret
	);"
	<apicall: long 'clGetPlatformInfo' (CLPlatformID ulong ulong void* ulong*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMAlloc__cl_context: context 
						cl_svm_mem_flags: flags 
						size_t: size 
						cl_uint: alignment 

	"This method was automatically generated."

	"void * clSVMAlloc(
			cl_context context, 
			cl_svm_mem_flags flags, 
			size_t size, 
			cl_uint alignment
	);"
	<apicall: void* 'clSVMAlloc' (CLContext ulonglong ulong ulong) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'FFI-generated-SVM Allocation APIs'!
clSVMFree__cl_context: context 
						void__ptr: svm_pointer 

	"This method was automatically generated."

	"void clSVMFree(
			cl_context context, 
			void * svm_pointer
	);"
	<apicall: void 'clSVMFree' (CLContext void*) module: 'OpenCL.dll'>

	^self externalCallFailed! !

!OpenCLWindows32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 5/17/2017 12:26:32'!
libraryForCurrentPlatform
	"Answer the name of the OpenCL library on the current platform."
	^'OpenCL.dll'! !

!OpenCLWindows32BitsInterface class methodsFor: 'class initialization' stamp: 'jmv 5/17/2017 12:26:32'!
supportedPlatform
	^'Win32'! !

!OpenCLRefCounter methodsFor: 'accessing' stamp: 'jcg 1/15/2010 01:39'!
count
	^refCount! !

!OpenCLRefCounter methodsFor: 'finalization' stamp: 'jcg 1/15/2010 01:41'!
finalize
	"Decrement the ref-count until it is zero."
	[refCount = 0] whileFalse: [self release].! !

!OpenCLRefCounter methodsFor: 'initialize' stamp: 'jcg 1/15/2010 01:01'!
id: anOpenCLOpaqueHandle
	id := anOpenCLOpaqueHandle.! !

!OpenCLRefCounter methodsFor: 'initialize' stamp: 'jcg 1/15/2010 00:50'!
initialize
	refCount := 1. "OpenCL implicitly retains all newly-created objects."! !

!OpenCLRefCounter methodsFor: 'initialize' stamp: 'jcg 6/7/2010 01:39'!
invalidateOnStartUp
	"Squeak is starting up, so our ref-counted object is invalid.  Set 'refCount' to zero so that any subsequent #release won't trigger any cleanup-behavior."
	refCount := 0.
! !

!OpenCLRefCounter methodsFor: 'retain/release' stamp: 'jcg 1/15/2010 01:21'!
release
	"Decrement the ref-count."
	refCount = 0 ifTrue: [^self "already completely released"].
	refCount := refCount - 1.
	id release.
! !

!OpenCLRefCounter methodsFor: 'retain/release' stamp: 'jcg 1/15/2010 01:20'!
retain
	"Increment the ref-count for this object so that OpenCL will not destroy it."
	refCount = 0 ifTrue: [^self "already completely released"].
	refCount := refCount + 1.
	id retain.! !

!OpenCLRefCounter class methodsFor: 'class initialization' stamp: 'jcg 6/7/2010 01:07'!
initialize
	"This functionality is now consolidated in CL class."
	"OpenCLRefCounter initialize"
	Smalltalk removeFromStartUpList: self.! !

!OpenCLExample methodsFor: 'accessing' stamp: 'jcg 5/1/2010 10:50'!
context
	"Answer the OpenCL context."
	^context! !

!OpenCLExample methodsFor: 'initialize' stamp: 'jcg 5/1/2010 11:30'!
destroy
	"Release any resources used by this example."
	device ifNotNil: [device := nil].
	context ifNotNil: [:obj |
		context := nil.
		obj destroy
	].
	queue ifNotNil: [:obj |
		queue := nil.
		obj destroy
	].
	program ifNotNil: [:obj |
		program := nil.
		obj destroy
	].
	! !

!OpenCLExample methodsFor: 'initialize' stamp: 'jmv 7/1/2019 17:50:45'!
initialize
	self destroy. "reset to initial state"
	device := OpenCLPlatform preferredDevice.
	context := OpenCLContext newForDevices: {device}.
	queue := context commandQueueOn: device.
	program := context programWithSource: self sourceCode.
	program build! !

!AppleCopyBenchmark methodsFor: 'accessing' stamp: 'jmv 1/5/2016 12:05'!
input: aFloatMatrix

	outputMatrix _ aFloatMatrix appropriateResultClass extent: aFloatMatrix extent.

	"Set up Kernels"
	kernel := program createKernel: 'copy'.
	inputBuffer := context readOnlyBufferOfSize: aFloatMatrix elements byteSize.
	outputBuffer := context writeOnlyBufferOfSize: outputMatrix elements byteSize.

	lastEvent := queue 
					writeBuffer: inputBuffer
					from: aFloatMatrix elements
					waitOn: lastEvent.

	kernel argAt: 0 putPointer: inputBuffer.
	kernel argAt: 1 putPointer: outputBuffer.
	kernel argAt: 2 putInt: outputMatrix width.
	kernel argAt: 3 putInt: outputMatrix height! !

!AppleCopyBenchmark methodsFor: 'accessing' stamp: 'jmv 9/1/2015 16:46'!
result

	queue readBuffer: outputBuffer into: outputMatrix elements waitOn: lastEvent.
	^outputMatrix! !

!AppleCopyBenchmark methodsFor: 'actions' stamp: 'jmv 9/1/2015 16:48'!
run

	lastEvent := queue 
			runKernel: kernel
			workDim: 2
			globalOffset: #(0 0)
			globalSize: {outputMatrix width. outputMatrix height}
			localSize: #(32 8)
			waitOn: lastEvent.! !

!AppleCopyBenchmark methodsFor: 'private' stamp: 'jmv 9/1/2015 16:14'!
sourceCode
	"Answer the OpenCL source code for this example."
	"Copy from / to global memory"
	^'
__kernel void copy(global const float * in, global float * out, int w, int h) {

	int x = get_global_id(0); 		// (x,y) = pixel to process
	int y = get_global_id(1); 		// in this work item

	out[x+y*w] = in[x+y*w]; 		// Load and Store

//	out[x+y*w] = 100*y+x;	
}
'! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 7/1/2016 16:49'!
downloadDataFromGPU: outputForm
	"Block, because outputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	
	"Öjo que origin y region tienen tamanio 3, no 2. Entender"
	
	lastEvent := queue
					readImage: result
					block: false
					origin: #(0 0 0)
					region: {resultExtent x. resultExtent y. 1}
					rowPitch: outputForm wordsPerLine * 4
					slicePitch: 0
					pointer: outputForm
					waitOn: lastEvent! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 9/9/2015 15:23'!
initialize

	"Set up context, queue, program, kernels, etc."
	super initialize.

	"Set up Kernels"
	kernel := program createKernel: 'resample'.
	
	"Create a buffer for the filter weights, big enough for a blur-radius of 100 pixels."
	" 
	stuff
	filterWeights := FloatArray new: 0.
	filterWeightsBuffer := context readOnlyBufferOfSize: 101 * 4.
	kernel argAt: 1 put: filterWeightsBuffer size: 4.
	" ! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2017 12:04:13'!
input: inputImage gridX: gridXPositions gridY: gridYPositions resultExtent: aResultExtent
"
Alternativas.
- Que aloque la memoria. despues le copio onda #uploadDataToGPU:
- Le digo ahora CL_MEM_COPY_HOST_PTR y le paso una imagen Smalltalk
- Le digo CL_MEM_COPY_HOST_PTR, pero uso un ExternalForm (quizas eso se pareceria mas a lo que voy a querer despues...)
"
"De que tipo son? Para hacer algo realista, la entrada y la salida deben ser de 8 bits. Lo de norm... no se.
Los grid deben ser Float, o al menos de 16 bits!!"
	| fmt |
"
Y esto que puse aca supone que en memoria son de 8 bits... Y al pasar hostPointer, como le digo que convierta?????? mhhhhhh
"
	"CL_UNORM_INT8   intensity8Norm
	Desde afuera, en la memoria hay un unsigned char, 0..255.
	Pero en el kernel uso read_imagef() y write_imagef(), son floats.
	http://stackoverflow.com/questions/31718492/meaning-of-cl-unorm-int8-for-cl-image-format-image-channel-data-type-and-its-di
	"
	
	"uhhhhhhh image format not supported....
	CLImageFormatNotSupported
	
	clGetSupportedImageFormats
	supportedImageFormats: flags type: imageType ' or 3D'
	https://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clGetSupportedImageFormats.html
	"
	" 
Ejecutar
OpenCLPlatform allPlatforms do: [ :platform | platform reportImageFormatsOn: Transcript ].

(	context supportedImageFormats: CL_MEM_READ_WRITE type: CL_MEM_OBJECT_IMAGE2D) do: [ :a | a print].
(	context supportedImageFormats: CL_MEM_WRITE_ONLY type: CL_MEM_OBJECT_IMAGE2D) do: [ :a | a print].
(	context supportedImageFormats: CL_MEM_READ_ONLY type: CL_MEM_OBJECT_IMAGE2D) do: [ :a | a print].
" 
"
CL_MEM_READ_WRITE
CL_MEM_WRITE_ONLY
CL_MEM_READ_ONLY
"

"
=================== 
I3 de la oficina (GPU vieja, sin OpenCL):
NO TIENE ningun formato de 8 bit por pixel!!
=================== 
#(an OpenCLPlatform)
a CLImageFormatStruct(RGBA unorm-int8)
a CLImageFormatStruct(RGBA unorm-int16)
a CLImageFormatStruct(RGBA int8)
a CLImageFormatStruct(RGBA int16)
a CLImageFormatStruct(RGBA int32)
a CLImageFormatStruct(RGBA uint8)
a CLImageFormatStruct(RGBA uint16)
a CLImageFormatStruct(RGBA uint32)
a CLImageFormatStruct(RGBA half-float)
a CLImageFormatStruct(RGBA float)
a CLImageFormatStruct(BGRA unorm-int8)
a CLImageFormatStruct(Intensity float)
a CLImageFormatStruct(Luminance float)
===============================
Celeron de casa (GPU mas nueva, con OpenCL)
No tiene Intensity o Luminance, pero al menos tiene 'R uint8'
=================== 
a CLImageFormatStruct(RGBA unorm-int8)
a CLImageFormatStruct(RGBA unorm-int16)
a CLImageFormatStruct(RGBA int8)
a CLImageFormatStruct(RGBA int16)
a CLImageFormatStruct(RGBA int32)
a CLImageFormatStruct(RGBA uint8)
a CLImageFormatStruct(RGBA uint16)
a CLImageFormatStruct(RGBA uint32)
a CLImageFormatStruct(RGBA half-float)
a CLImageFormatStruct(RGBA float)
a CLImageFormatStruct(BGRA unorm-int8)
a CLImageFormatStruct(R float)
a CLImageFormatStruct(R unorm-int8)
a CLImageFormatStruct(R unorm-int16)
a CLImageFormatStruct(R int8)
a CLImageFormatStruct(R int16)
a CLImageFormatStruct(R int32)
a CLImageFormatStruct(R uint8)
a CLImageFormatStruct(R uint16)
a CLImageFormatStruct(R uint32)
a CLImageFormatStruct(R half-float)
a CLImageFormatStruct(A unorm-int8)
a CLImageFormatStruct(RG unorm-int8)
a CLImageFormatStruct(RG unorm-int16)
a CLImageFormatStruct(RG int8)
a CLImageFormatStruct(RG int16)
a CLImageFormatStruct(RG int32)
a CLImageFormatStruct(RG uint8)
a CLImageFormatStruct(RG uint16)
a CLImageFormatStruct(RG uint32)
a CLImageFormatStruct(RG half-float)
a CLImageFormatStruct(RG float)
================================
En la AMD A4-5000,
OpenCLPlatform allPlatforms do: [ :platform |
    '-----' print.
    platform name print.
    ((OpenCLContext newForDevices: platform allGPUs, platform allCPUs) supportedImageFormats: CL_MEM_READ_ONLY type: 16r10F1) do: [ :each |
        each print ]].
==================
AMD Accelerated Parallel Processing
a CLImageFormatStruct(R snorm-int8)
a CLImageFormatStruct(R snorm-int16)
a CLImageFormatStruct(R unorm-int8)
a CLImageFormatStruct(R unorm-int16)
a CLImageFormatStruct(R int8)
a CLImageFormatStruct(R int16)
a CLImageFormatStruct(R int32)
a CLImageFormatStruct(R uint8)
a CLImageFormatStruct(R uint16)
a CLImageFormatStruct(R uint32)
a CLImageFormatStruct(R half-float)
a CLImageFormatStruct(R float)
a CLImageFormatStruct(A snorm-int8)
a CLImageFormatStruct(A snorm-int16)
a CLImageFormatStruct(A unorm-int8)
a CLImageFormatStruct(A unorm-int16)
a CLImageFormatStruct(A int8)
a CLImageFormatStruct(A int16)
a CLImageFormatStruct(A int32)
a CLImageFormatStruct(A uint8)
a CLImageFormatStruct(A uint16)
a CLImageFormatStruct(A uint32)
a CLImageFormatStruct(A half-float)
a CLImageFormatStruct(A float)
a CLImageFormatStruct(RG snorm-int8)
a CLImageFormatStruct(RG snorm-int16)
a CLImageFormatStruct(RG unorm-int8)
a CLImageFormatStruct(RG unorm-int16)
a CLImageFormatStruct(RG int8)
a CLImageFormatStruct(RG int16)
a CLImageFormatStruct(RG int32)
a CLImageFormatStruct(RG uint8)
a CLImageFormatStruct(RG uint16)
a CLImageFormatStruct(RG uint32)
a CLImageFormatStruct(RG half-float)
a CLImageFormatStruct(RG float)
a CLImageFormatStruct(RGBA snorm-int8)
a CLImageFormatStruct(RGBA snorm-int16)
a CLImageFormatStruct(RGBA unorm-int8)
a CLImageFormatStruct(RGBA unorm-int16)
a CLImageFormatStruct(RGBA int8)
a CLImageFormatStruct(RGBA int16)
a CLImageFormatStruct(RGBA int32)
a CLImageFormatStruct(RGBA uint8)
a CLImageFormatStruct(RGBA uint16)
a CLImageFormatStruct(RGBA uint32)
a CLImageFormatStruct(RGBA half-float)
a CLImageFormatStruct(RGBA float)
a CLImageFormatStruct(ARGB snorm-int8)
a CLImageFormatStruct(ARGB unorm-int8)
a CLImageFormatStruct(ARGB int8)
a CLImageFormatStruct(ARGB uint8)
a CLImageFormatStruct(BGRA snorm-int8)
a CLImageFormatStruct(BGRA unorm-int8)
a CLImageFormatStruct(BGRA int8)
a CLImageFormatStruct(BGRA uint8)
a CLImageFormatStruct(Luminance snorm-int8)
a CLImageFormatStruct(Luminance snorm-int16)
a CLImageFormatStruct(Luminance unorm-int8)
a CLImageFormatStruct(Luminance unorm-int16)
a CLImageFormatStruct(Luminance half-float)
a CLImageFormatStruct(Luminance float)
a CLImageFormatStruct(Intensity snorm-int8)
a CLImageFormatStruct(Intensity snorm-int16)
a CLImageFormatStruct(Intensity unorm-int8)
a CLImageFormatStruct(Intensity unorm-int16)
a CLImageFormatStruct(Intensity half-float)
a CLImageFormatStruct(Intensity float)
a CLImageFormatStruct(RA snorm-int8)
a CLImageFormatStruct(RA snorm-int16)
a CLImageFormatStruct(RA unorm-int8)
a CLImageFormatStruct(RA unorm-int16)
a CLImageFormatStruct(RA int8)
a CLImageFormatStruct(RA int16)
a CLImageFormatStruct(RA int32)
a CLImageFormatStruct(RA uint8)
a CLImageFormatStruct(RA uint16)
a CLImageFormatStruct(RA uint32)
a CLImageFormatStruct(RA half-float)
a CLImageFormatStruct(RA float)
#(an OpenCLPlatform)
"
	
	
	fmt := CLImageFormatStruct bgra8888norm.
	fmt := CLImageFormatStruct luminance8.
"	fmt := CLImageFormatStruct r8Norm.
	fmt := CLImageFormatStruct intensityFloat."
	fmt := CLImageFormatStruct r8.
	input := context 
							newImage2D: CL_MEM_READ_ONLY
							format: fmt
							width: inputImage width
							height: inputImage height
							rowPitch: nil
							hostPointer: nil.
							


							
	resultExtent _ aResultExtent.
	outputImage _ (Smalltalk at: #FloatImage) extent: aResultExtent.
	fmt := CLImageFormatStruct intensity8Norm.
	input := context 
					newImage2D: CL_MEM_READ_ONLY "+ CL_MEM_COPY_HOST_PTR"
					format: fmt
					width: inputImage width
					height: inputImage height
					rowPitch: nil
					hostPointer: "(inputImage elements collect: [ :v | (v * 256) floor min: 255 max: 0 ] into: ByteArray)" nil.
"
	gridX := context 
					newImage2D: CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR
					format: fmt
					width: gridXPositions width
					height: gridXPositions height
					rowPitch: nil
					hostPointer: gridXPositions asOpenCLArg.
	gridY := context 
					newImage2D: CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR
					format: fmt
					width: gridYPositions width
					height: gridYPositions height
					rowPitch: nil
					hostPointer: gridYPositions asOpenCLArg.
"
	fmt := CLImageFormatStruct r8.
	result := context 
					newImage2D: CL_MEM_WRITE_ONLY
					format: fmt
					width: resultExtent x
					height: resultExtent y
					rowPitch: nil
					hostPointer: nil.! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 9/3/2015 16:59'!
outputImage
^outputImage! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 9/3/2015 16:58'!
run
	"Compute work sizes."
	| globalWorkSize localWorkSize |
	localWorkSize := #(16 16).  "should fit on most devices... see #maxWorkGroupSize"
	globalWorkSize := {resultExtent x. resultExtent y}.
	globalWorkSize := globalWorkSize 
							with: localWorkSize 
							collect: [:global :local | global roundUpTo: local].		
					
	"Run the two kernels to blur the image first horizontally, then vertically."
	lastEvent := queue
					runKernel: kernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
					
	self downloadDataFromGPU: outputImage! !

!DeformationGridExample methodsFor: 'as yet unclassified' stamp: 'jmv 9/3/2015 17:13'!
sourceCode
	"Answer the OpenCL source code for the guassian-blur program."
	"Sampleamos el resultado de manera discreta"
^'

	__constant sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

	__kernel void resample (
			__read_only image2d_t src_image,
			__read_only image2d_t gridX_image,
			__read_only image2d_t gridY_image,
			__write_only image2d_t dst_image) {

		if (get_global_id(0) >= get_image_width(dst_image)) return;
		if (get_global_id(1) >= get_image_height(dst_image)) return;

		int2 coord = (int2)(get_global_id(0), get_global_id(1));
		float4 out = read_imagef(src_image, sampler, coord);
		write_imagef(dst_image, coord, out);
	}'! !

!DeformationGridExample class methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2017 12:03:39'!
dale
	"
	DeformationGridExample dale
	Experimental. Unfinished. Requires SignalProcessing.pck.st
	En casa aun no puedo instanciar la de resultado...
	"
	| experiment gridXPositions gridYPositions inputImage resultExtent floatImageClass |
	experiment _ self new.
floatImageClass _ Smalltalk at: #FloatImage.
inputImage _ floatImageClass lena.
inputImage _ floatImageClass grid: 12@12 extent: 256@256.
resultExtent _ 512@512.
gridXPositions _ floatImageClass fromArrayOfArrays: #(
	(0.0 0.3 1.0)
	(0.0 0.6 1.0)
	(0.0 0.8 1.0)).
gridYPositions _ floatImageClass fromArrayOfArrays: #(
	(0.0 0.3 0.0)
	(0.7 0.6 0.5)
	(1.0 1.0 1.0)).

experiment input: inputImage gridX: gridXPositions gridY: gridYPositions resultExtent: resultExtent.
"
result replaceValues: [ :y :x :v |
	yy _ y-1.0/(result height-1) * (gridXPositions height-1) + 1.
	xx _ x-1.0/(result width-1) * (gridXPositions width-1) + 1.
	xp _ (gridXPositions bilinearInterpolatedValueAtX: xx y: yy) * 255+1.
	yp _ (gridYPositions bilinearInterpolatedValueAtX: xx y: yy) * 255+1.
	inputImage bilinearInterpolatedValueAtX: xp y: yp ].
"
experiment run.
experiment outputImage display.
	
	
	"
input _ FloatImage lena.
input _ FloatImage grid: 12@12 extent: 256@256.
result _ FloatImage extent: 500@500.
gridX _ FloatImage fromArrayOfArrays: #(
	(0.0 0.3 1.0)
	(0.0 0.6 1.0)
	(0.0 0.8 1.0)).
gridY _ FloatImage fromArrayOfArrays: #(
	(0.0 0.3 0.0)
	(0.7 0.6 0.5)
	(1.0 1.0 1.0)).

result replaceValues: [ :y :x :v |
	yy _ y-1.0/(result height-1) * (gridX height-1) + 1.
	xx _ x-1.0/(result width-1) * (gridX width-1) + 1.
	xp _ (gridX bilinearInterpolatedValueAtX: xx y: yy) * 255+1.
	yp _ (gridY bilinearInterpolatedValueAtX: xx y: yy) * 255+1.
	input bilinearInterpolatedValueAtX: xp y: yp ].
result display
"! !

!NormalizedCoordinatesExperiment methodsFor: 'private' stamp: 'jmv 7/1/2016 16:44'!
downloadDataFromGPU: outputForm
	"Block, because outputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					readImage: processedFrame
					block: false
					origin: #(0 0 0)
					region: {outputForm width. outputForm height. 1}
					rowPitch: outputForm wordsPerLine * 4
					slicePitch: 0
					pointer: outputForm
					waitOn: lastEvent! !

!NormalizedCoordinatesExperiment methodsFor: 'private' stamp: 'jmv 2/22/2016 10:46'!
setupInputImage: inputForm
	"Create new OpenCL images to perform the blur, and assign them as arguments to both kernels.
	Use the OpenCL 1.2 api for this."
	| format description |

	"r8Norm para que en el kernel se pueda usar read_imagef() y write_imagef().
	Ver OpenCL reference '6.11.8.2 Built-in Image Functions'  p.192
	#r8Norm, #luminance8Norm, #intensity8Norm parecen dar lo mismo"
	format _ CLImageFormatStruct r8Norm.
"	format _ CLImageFormatStruct luminance8Norm."
"	format _ CLImageFormatStruct intensity8Norm."

	description _ CLImageDescStruct new.
	description  		"https://www.khronos.org/registry/cl/sdk/1.2/docs/man/xhtml/imageDescriptor.html"
		image_type: CL_MEM_OBJECT_IMAGE2D;
		image_width: inputForm width;
		image_height: inputForm height;
		image_depth: 0;				"Only used for 3d images"
		image_array_size: 0; 		"This is only used if the image is a 1D or 2D image array"
		image_row_pitch: 0;
		image_slice_pitch: 0;
		num_mip_levels: 0;
		num_samples: 0;
		buffer: 0.
	"Image creation the OpenCL 1.2 way"
	originalFrame _ context 
		newImage: CL_MEM_READ_ONLY
		format: format
		description: description
		hostPointer: nil! !

!NormalizedCoordinatesExperiment methodsFor: 'private' stamp: 'jmv 2/22/2016 10:59'!
setupOutputImage: outputForm
	"Create new OpenCL images to perform the blur, and assign them as arguments to both kernels.
	Use the OpenCL 1.2 api for this."
	| format description |

	"r8Norm para que en el kernel se pueda usar read_imagef() y write_imagef().
	Ver OpenCL reference '6.11.8.2 Built-in Image Functions'  p.192
	#r8Norm, #luminance8Norm, #intensity8Norm parecen dar lo mismo"
	format _ CLImageFormatStruct r8Norm.
"	format _ CLImageFormatStruct luminance8Norm."
"	format _ CLImageFormatStruct intensity8Norm."

	description _ CLImageDescStruct new.
	description  		"https://www.khronos.org/registry/cl/sdk/1.2/docs/man/xhtml/imageDescriptor.html"
		image_type: CL_MEM_OBJECT_IMAGE2D;
		image_width: outputForm width;
		image_height: outputForm height;
		image_depth: 0;				"Only used for 3d images"
		image_array_size: 0; 		"This is only used if the image is a 1D or 2D image array"
		image_row_pitch: 0;
		image_slice_pitch: 0;
		num_mip_levels: 0;
		num_samples: 0;
		buffer: 0.
	"Image creation the OpenCL 1.2 way"
	processedFrame _ context 
		newImage: CL_MEM_WRITE_ONLY 
		format: format
		description: description
		hostPointer: nil! !

!NormalizedCoordinatesExperiment methodsFor: 'private' stamp: 'jmv 2/22/2016 12:29'!
sourceCode
	"Answer the OpenCL source code for the program."
"	Todos los kernels juntos en cada linea.
	Si de casualidad le pego a la rerrida de OpenCL, entonces me queda en orden la recorrida de RAM"
^'

	//__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_NONE | CLK_FILTER_LINEAR;
	__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_LINEAR;
	//__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_CLAMP | CLK_FILTER_LINEAR;
	//__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_REPEAT | CLK_FILTER_LINEAR;
	//__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_MIRRORED_REPEAT | CLK_FILTER_LINEAR;

	__kernel void the_kernel (
							__read_only image2d_t src_image,
							__write_only image2d_t dst_image)
	{

		float scaleH = 1.0 / get_image_width(dst_image);
		float scaleV = 1.0 / get_image_height(dst_image);

		float2 inputCoord = (float2)(get_global_id(0) * scaleH , get_global_id(1) * scaleV);
		int2 outputCoord = (int2)(get_global_id(0), get_global_id(1));

		float value = read_imagef(src_image, sampler, inputCoord).x;

		write_imagef(dst_image, outputCoord, (float4)(value,0,0,0));

	}'! !

!NormalizedCoordinatesExperiment methodsFor: 'private' stamp: 'jmv 7/1/2016 16:44'!
uploadDataToGPU: inputForm
	"Block, because inputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					writeImage: originalFrame
					block: false
					origin: #(0 0 0)
					region: {inputForm width. inputForm height. 1}
					rowPitch: inputForm wordsPerLine * 4
					slicePitch: 0
					pointer: inputForm
					waitOn: lastEvent! !

!NormalizedCoordinatesExperiment methodsFor: 'initialize' stamp: 'jmv 2/22/2016 10:46'!
initialize

	"Set up context, queue, program, kernels, etc."
	super initialize.

	"Set up Kernels"
	kernel := program createKernel: 'the_kernel'! !

!NormalizedCoordinatesExperiment methodsFor: 'actions' stamp: 'jmv 2/22/2016 11:00'!
process: inputForm into: outputForm
	| localWorkSize globalWorkSize startTime |
	
	"Sanity check."
	inputForm depth = 8 ifFalse: [self error: 'input form depth must be 8'].
	outputForm depth = 8 ifFalse: [self error: 'output form depth must be 8'].

	self setupInputImage: inputForm.
	kernel argAt: 0 putPointer: originalFrame.
	self setupOutputImage: outputForm.
	kernel argAt: 1 putPointer: processedFrame.
	'---------------------' print.

	startTime := Time millisecondClockValue.
	"Upload image to the GPU so we can process it." 
	self uploadDataToGPU: inputForm.
	OpenCLEvent waitForEvents: lastEvent.
	((Time millisecondClockValue - startTime) printString, ' ms to Upload to GPU') print.
					
	"Compute work sizes."
	localWorkSize := #(256 1).  "should fit on most devices... see #maxWorkGroupSize. Minimum number that already gives max performance"
	globalWorkSize := {outputForm width. outputForm height}.

	startTime := Time millisecondClockValue.
	lastEvent := queue
					runKernel: kernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
	OpenCLEvent waitForEvents: lastEvent.
	((Time millisecondClockValue - startTime) printString, ' ms to Run Kernel') print.
					
	"Retrieve result image from the GPU."
	startTime := Time millisecondClockValue.
	self downloadDataFromGPU: outputForm.
	OpenCLEvent waitForEvents: lastEvent.
	((Time millisecondClockValue - startTime) printString, ' ms to Download from GPU') print.! !

!NormalizedCoordinatesExperiment class methodsFor: 'example' stamp: 'jmv 5/12/2016 18:34'!
go
	"
	NormalizedCoordinatesExperiment go
	"
	| algoritm src dest value |
	algoritm _ self new.

	src _ GrayForm extent: 4@4.
	
	0 to: src width-1 do: [ :x |
		value _ (x * 255.0 / (src width-1)) rounded.
		value _ (x+0.5 * 255.0 / (src width)) rounded print.
		0 to: src height-1 do: [ :y |
			src pixelValueAt: x@y put: value.
			]].
	dest _ GrayForm extent: 300@1.
	algoritm process: src into: dest.
	dest displayAt: 0@0.

	{src. dest} explore.
	dest bits asByteArray print! !

!OpenCLBlur methodsFor: 'actions' stamp: 'jmv 5/16/2017 14:44:13'!
blur: inputForm into: outputForm radius: filterRadius
	| localWorkSize globalWorkSize |
	
	"Sanity check."
	inputForm extent = outputForm extent ifFalse: [self error: 'image sizes do not match'].
	inputForm depth = 32 ifFalse: [self error: 'input form depth must be 32'].
	outputForm depth = 32 ifFalse: [self error: 'output form depth must be 32'].

	inputForm extent = imageExtent ifFalse: [
		"Need to allocate new images."
		self setupImages: (imageExtent := inputForm extent).
	].

	(filterWeights size - 1) = filterRadius ifFalse: [
		"Need to compute and set-up new filter weights."
		self setupWeightsForRadius: filterRadius.
	].
	
	"Upload image to the GPU so we can blur it." 
	self uploadDataToGPU: inputForm.
					
	"Compute work sizes."
	localWorkSize := #(16 16).  "should fit on most devices... see #maxWorkGroupSize"
	localWorkSize := #(128 1).  "Largest that works on tests done on MacOS with no GPU."
	globalWorkSize := {imageExtent x. imageExtent y}.
	globalWorkSize := globalWorkSize 
							with: localWorkSize 
							collect: [:global :local | global roundUpTo: local].		
					
	"Run the two kernels to blur the image first horizontally, then vertically."
	lastEvent := queue
					runKernel: horizontalKernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
	lastEvent := queue
					runKernel: verticalKernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
					
	"Retrieve blurred image from the GPU."				
	self downloadDataFromGPU: outputForm.
	
	"When this event is finished, 'outputForm' will have its new contents.  Yay!!"				
	^lastEvent ! !

!OpenCLBlur methodsFor: 'actions' stamp: 'jcg 2/16/2010 02:00'!
blur: inputAndOutputForm radius: filterRadius
	"Input and output image are the same."
	^self blur: inputAndOutputForm into: inputAndOutputForm radius: filterRadius
	! !

!OpenCLBlur methodsFor: 'private' stamp: 'jmv 7/1/2016 16:49'!
downloadDataFromGPU: outputForm
	"Block, because outputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					readImage: inputOutputImage
					block: false
					origin: #(0 0 0)
					region: {imageExtent x. imageExtent y. 1}
					rowPitch: outputForm wordsPerLine * 4
					slicePitch: 0
					pointer: outputForm
					waitOn: lastEvent! !

!OpenCLBlur methodsFor: 'private' stamp: 'jmv 1/5/2016 11:54'!
setupImages: extent
	"Create new OpenCL images to perform the blur, and assign them as arguments to both kernels."
	| fmt |
	fmt := CLImageFormatStruct bgra8888norm.
	inputOutputImage := context 
							newImage2D: CL_MEM_READ_WRITE
							format: fmt
							width: extent x
							height: extent y
							rowPitch: nil
							hostPointer: nil.
	tempImage := context 
							newImage2D: CL_MEM_READ_WRITE
							format: fmt
							width: extent x
							height: extent y
							rowPitch: nil
							hostPointer: nil.
	
	"Horizontal kernel runs first, storing the horizontally-blurred image into 'tempImage'."						
	horizontalKernel argAt: 2 putPointer: inputOutputImage.
	horizontalKernel argAt: 3 putPointer: tempImage.
	
	"Vertical kernel runs next, using the partly-blurred image in 'tempImage' as the source."
	verticalKernel argAt: 2 putPointer: tempImage.
	verticalKernel argAt: 3 putPointer: inputOutputImage! !

!OpenCLBlur methodsFor: 'private' stamp: 'jmv 9/3/2020 17:12:44'!
setupWeightsForRadius: filterRadius
	| sigma sum |
	
	filterRadius isInteger ifFalse: [self error: 'filter radius must be an integer'].
	filterRadius > 100 ifTrue: [
		"In #initialize, we only allocated enough space for 101 filter weights."
		self error: 'filter radius must be <= 100 pixels'.
	].

	"If the radius is zero, there is no blurring."
	filterRadius = 0 ifTrue: [
		filterWeights := Float32Array with: 1.0.
		^self.
	].
	
	filterWeights := Float32Array new: filterRadius + 1.
	sigma := filterRadius / 3.0.  "past 3 std-deviations, Gaussian is near-zero."
	
	"Compute unnormalized gaussian weights."
	0 to: filterRadius do: [:i |
		| t1 t2 |
		t1 := 1 / (sigma * (Float pi * 2) sqrt).
		t2 := (i / sigma) squared * -0.5.
		filterWeights at: i+1 put: (t1 * (Float e raisedTo: t2))
	].

	"Normalize.  We've computed half a bell curve, so count some weights twice so that the full curve sums to 1."
	sum := filterWeights sum * 2.
	sum := sum - (filterWeights first).  "only count the center of the curve once."
	filterWeights /= sum.
	
	"Upload filter weights to buffer."
	lastEvent := queue 
					writeBuffer: filterWeightsBuffer
					from: filterWeights
					waitOn: lastEvent.
	
	"Set up kernel arguments."
	horizontalKernel argAt: 0 putInt: filterRadius.
	verticalKernel argAt: 0 putInt: filterRadius

	! !

!OpenCLBlur methodsFor: 'private' stamp: 'jmv 8/10/2015 12:11'!
sourceCode
	"Answer the OpenCL source code for the guassian-blur program."
^'

	__constant sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

	__kernel void
	image_filter_horizontal (int filterHalfWidth,
							__constant float* filterWeights,  // of size filterHalfWidth + 1
							__read_only image2d_t src_image,
							__write_only image2d_t dst_image)
	{
		if (get_global_id(0) >= get_image_width(dst_image)) return;
		if (get_global_id(1) >= get_image_height(dst_image)) return;
		int i;
		int2 coord = (int2)(get_global_id(0), get_global_id(1));
		float4 weight = (float4)filterWeights[0];
		float4 out = read_imagef(src_image, sampler, coord) * weight;
		for (i=1; i <= filterHalfWidth; i++) {
			weight = (float4)filterWeights[i];
			out += (read_imagef(src_image, sampler, (coord + (int2)(i,0))) * weight);
			out += (read_imagef(src_image, sampler, (coord - (int2)(i,0))) * weight);
		}
		write_imagef(dst_image, coord, out);
	}
	
	__kernel void
	image_filter_vertical (int filterHalfHeight,
						__constant float* filterWeights, // of size filterHalfHeight + 1
						__read_only image2d_t src_image,
						__write_only image2d_t dst_image)
	{
		if (get_global_id(0) >= get_image_width(dst_image)) return;
		if (get_global_id(1) >= get_image_height(dst_image)) return;
		int i;
		int2 coord = (int2)(get_global_id(0), get_global_id(1));
		float4 weight = (float4)filterWeights[0];
		float4 out = read_imagef(src_image, sampler, coord) * weight;
		for (i=1; i <= filterHalfHeight; i++) {
			weight = (float4)filterWeights[i];
			out += (read_imagef(src_image, sampler, (coord + (int2)(0,i))) * weight);
			out += (read_imagef(src_image, sampler, (coord - (int2)(0,i))) * weight);
		}
		write_imagef(dst_image, coord, out);
	}'! !

!OpenCLBlur methodsFor: 'private' stamp: 'jmv 7/1/2016 16:52'!
uploadDataToGPU: inputForm
	"Block, because inputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					writeImage: inputOutputImage
					block: false
					origin: #(0 0 0)
					region: {imageExtent x. imageExtent y. 1}
					rowPitch: inputForm wordsPerLine * 4
					slicePitch: 0
					pointer: inputForm
					waitOn: lastEvent! !

!OpenCLBlur methodsFor: 'initialize' stamp: 'jmv 9/3/2020 17:12:44'!
initialize

	"Set up context, queue, program, kernels, etc."
	super initialize.

	"Set up Kernels"
	horizontalKernel := program createKernel: 'image_filter_horizontal'.
	verticalKernel := program createKernel: 'image_filter_vertical'.
	
	"Create a buffer for the filter weights, big enough for a blur-radius of 100 pixels."
	filterWeights := Float32Array new: 0.  "will be replaced when the blur-radius is set"
	filterWeightsBuffer := context readOnlyBufferOfSize: 101 * 4.
	horizontalKernel argAt: 1 putPointer: filterWeightsBuffer.
	verticalKernel argAt: 1 putPointer: filterWeightsBuffer! !

!OpenCLBlur class methodsFor: 'example' stamp: 'jmv 5/4/2016 12:58'!
blurDisplay
	"
	OpenCLBlur blurDisplay
	"
	| blur src dest |
	blur := self new.
	
	dest := ExternalForm extent: Display extent nativeDepthFrom: 32.
	dest allocateSpace.
	src := ExternalForm extent: Display extent nativeDepthFrom: 32.
	src allocateSpace.
	Display displayOn: src.
		
	((1 to: 20), (19 to: 1 by: -1)) do: [:radius |
		| evt startTime queuedTime finishTime |
		startTime := Time millisecondClockValue.
		evt := blur blur: src into: dest radius: radius.
		queuedTime := Time millisecondClockValue - startTime.
		OpenCLEvent waitForEvents: evt.
		finishTime := Time millisecondClockValue - startTime.
		('   QUEUED COMMANDS IN: ', queuedTime printString, 'msecs     ') print.
		('   FINISHED COMMANDS IN: ', finishTime printString, 'msecs     ') print.
		'   QUEUED COMMANDS IN: ', queuedTime printString, 'msecs     ' displayOn: dest at: 20@20.
		'   FINISHED COMMANDS IN: ', finishTime printString, 'msecs     ' displayOn: dest at: 20@40.
		dest displayAt: 0@0.

		(Delay forMilliseconds: 20) wait.
	].
	src displayAt: 0@0.
! !

!OpenCLBlur12 methodsFor: 'actions' stamp: 'jmv 5/16/2017 14:44:09'!
blur: inputForm into: outputForm radius: filterRadius
	| localWorkSize globalWorkSize |
	
	"Sanity check."
	inputForm extent = outputForm extent ifFalse: [self error: 'image sizes do not match'].
	inputForm depth = 32 ifFalse: [self error: 'input form depth must be 32'].
	outputForm depth = 32 ifFalse: [self error: 'output form depth must be 32'].

	inputForm extent = imageExtent ifFalse: [
		"Need to allocate new images."
		self setupImages: (imageExtent := inputForm extent).
	].

	(filterWeights size - 1) = filterRadius ifFalse: [
		"Need to compute and set-up new filter weights."
		self setupWeightsForRadius: filterRadius.
	].
	
	"Upload image to the GPU so we can blur it." 
	self uploadDataToGPU: inputForm.
					
	"Compute work sizes."
	localWorkSize := #(16 16).  "should fit on most devices... see #maxWorkGroupSize"
	localWorkSize := #(128 1).  "Largest that works on tests done on MacOS with no GPU."
	globalWorkSize := {imageExtent x. imageExtent y}.
	globalWorkSize := globalWorkSize 
							with: localWorkSize 
							collect: [:global :local | global roundUpTo: local].		
					
	"Run the two kernels to blur the image first horizontally, then vertically."
	lastEvent := queue
					runKernel: horizontalKernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
	lastEvent := queue
					runKernel: verticalKernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: globalWorkSize
					localSize: localWorkSize 
					waitOn: lastEvent.
					
	"Retrieve blurred image from the GPU."				
	self downloadDataFromGPU: outputForm.
	
	"When this event is finished, 'outputForm' will have its new contents.  Yay!!"				
	^lastEvent ! !

!OpenCLBlur12 methodsFor: 'actions' stamp: 'jmv 10/29/2015 12:47'!
blur: inputAndOutputForm radius: filterRadius
	"Input and output image are the same."
	^self blur: inputAndOutputForm into: inputAndOutputForm radius: filterRadius
	! !

!OpenCLBlur12 methodsFor: 'private' stamp: 'jmv 7/1/2016 16:49'!
downloadDataFromGPU: outputForm
	"Block, because outputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					readImage: inputOutputImage
					block: false
					origin: #(0 0 0)
					region: {imageExtent x. imageExtent y. 1}
					rowPitch: outputForm wordsPerLine * 4
					slicePitch: 0
					pointer: outputForm
					waitOn: lastEvent! !

!OpenCLBlur12 methodsFor: 'private' stamp: 'jmv 1/5/2016 11:58'!
setupImages: extent
	"Create new OpenCL images to perform the blur, and assign them as arguments to both kernels.
	Use the OpenCL 1.2 api for this."
	| format description |
	format _ CLImageFormatStruct bgra8888norm.
	description _ CLImageDescStruct new.
	description  		"https://www.khronos.org/registry/cl/sdk/1.2/docs/man/xhtml/imageDescriptor.html"
		image_type: CL_MEM_OBJECT_IMAGE2D;
		image_width: extent x;
		image_height: extent y;
		image_depth: 0;				"Only used for 3d images"
		image_array_size: 0; 		"This is only used if the image is a 1D or 2D image array"
		image_row_pitch: 0;
		image_slice_pitch: 0;
		num_mip_levels: 0;
		num_samples: 0;
		buffer: 0.
	"Image creation the OpenCL 1.2 way"
	inputOutputImage := context 
							newImage: CL_MEM_READ_WRITE
							format: format
							description: description
							hostPointer: nil.
	tempImage := context 
							newImage: CL_MEM_READ_WRITE
							format: format
							description: description
							hostPointer: nil.
	
	"Horizontal kernel runs first, storing the horizontally-blurred image into 'tempImage'."						
	horizontalKernel argAt: 2 putPointer: inputOutputImage.
	horizontalKernel argAt: 3 putPointer: tempImage.
	
	"Vertical kernel runs next, using the partly-blurred image in 'tempImage' as the source."
	verticalKernel argAt: 2 putPointer: tempImage.
	verticalKernel argAt: 3 putPointer: inputOutputImage! !

!OpenCLBlur12 methodsFor: 'private' stamp: 'jmv 9/3/2020 17:12:45'!
setupWeightsForRadius: filterRadius
	| sigma sum |
	
	filterRadius isInteger ifFalse: [self error: 'filter radius must be an integer'].
	filterRadius > 100 ifTrue: [
		"In #initialize, we only allocated enough space for 101 filter weights."
		self error: 'filter radius must be <= 100 pixels'.
	].

	"If the radius is zero, there is no blurring."
	filterRadius = 0 ifTrue: [
		filterWeights := Float32Array with: 1.0.
		^self.
	].
	
	filterWeights := Float32Array new: filterRadius + 1.
	sigma := filterRadius / 3.0.  "past 3 std-deviations, Gaussian is near-zero."
	
	"Compute unnormalized gaussian weights."
	0 to: filterRadius do: [:i |
		| t1 t2 |
		t1 := 1 / (sigma * (Float pi * 2) sqrt).
		t2 := (i / sigma) squared * -0.5.
		filterWeights at: i+1 put: (t1 * (Float e raisedTo: t2))
	].

	"Normalize.  We've computed half a bell curve, so count some weights twice so that the full curve sums to 1."
	sum := filterWeights sum * 2.
	sum := sum - (filterWeights first).  "only count the center of the curve once."
	filterWeights /= sum.
	
	"Upload filter weights to buffer."
	lastEvent := queue 
					writeBuffer: filterWeightsBuffer
					from: filterWeights
					waitOn: lastEvent.
	
	"Set up kernel arguments."
	horizontalKernel argAt: 0 putInt: filterRadius.
	verticalKernel argAt: 0 putInt: filterRadius.

	! !

!OpenCLBlur12 methodsFor: 'private' stamp: 'jmv 10/29/2015 12:47'!
sourceCode
	"Answer the OpenCL source code for the guassian-blur program."
^'

	__constant sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

	__kernel void
	image_filter_horizontal (int filterHalfWidth,
							__constant float* filterWeights,  // of size filterHalfWidth + 1
							__read_only image2d_t src_image,
							__write_only image2d_t dst_image)
	{
		if (get_global_id(0) >= get_image_width(dst_image)) return;
		if (get_global_id(1) >= get_image_height(dst_image)) return;
		int i;
		int2 coord = (int2)(get_global_id(0), get_global_id(1));
		float4 weight = (float4)filterWeights[0];
		float4 out = read_imagef(src_image, sampler, coord) * weight;
		for (i=1; i <= filterHalfWidth; i++) {
			weight = (float4)filterWeights[i];
			out += (read_imagef(src_image, sampler, (coord + (int2)(i,0))) * weight);
			out += (read_imagef(src_image, sampler, (coord - (int2)(i,0))) * weight);
		}
		write_imagef(dst_image, coord, out);
	}
	
	__kernel void
	image_filter_vertical (int filterHalfHeight,
						__constant float* filterWeights, // of size filterHalfHeight + 1
						__read_only image2d_t src_image,
						__write_only image2d_t dst_image)
	{
		if (get_global_id(0) >= get_image_width(dst_image)) return;
		if (get_global_id(1) >= get_image_height(dst_image)) return;
		int i;
		int2 coord = (int2)(get_global_id(0), get_global_id(1));
		float4 weight = (float4)filterWeights[0];
		float4 out = read_imagef(src_image, sampler, coord) * weight;
		for (i=1; i <= filterHalfHeight; i++) {
			weight = (float4)filterWeights[i];
			out += (read_imagef(src_image, sampler, (coord + (int2)(0,i))) * weight);
			out += (read_imagef(src_image, sampler, (coord - (int2)(0,i))) * weight);
		}
		write_imagef(dst_image, coord, out);
	}'! !

!OpenCLBlur12 methodsFor: 'private' stamp: 'jmv 7/1/2016 16:52'!
uploadDataToGPU: inputForm
	"Block, because inputForm may be moved by GC before the data transfer occurs.  We could test to see if we're using an ExternalForm, and if so use a non-blocking command."
	lastEvent := queue
					writeImage: inputOutputImage
					block: false
					origin: #(0 0 0)
					region: {imageExtent x. imageExtent y. 1}
					rowPitch: inputForm wordsPerLine * 4
					slicePitch: 0
					pointer: inputForm
					waitOn: lastEvent! !

!OpenCLBlur12 methodsFor: 'initialize' stamp: 'jmv 9/3/2020 17:12:44'!
initialize

	"Set up context, queue, program, kernels, etc."
	super initialize.

	"Set up Kernels"
	horizontalKernel := program createKernel: 'image_filter_horizontal'.
	verticalKernel := program createKernel: 'image_filter_vertical'.
	
	"Create a buffer for the filter weights, big enough for a blur-radius of 100 pixels."
	filterWeights := Float32Array new: 0.  "will be replaced when the blur-radius is set"
	filterWeightsBuffer := context readOnlyBufferOfSize: 101 * 4.
	horizontalKernel argAt: 1 putPointer: filterWeightsBuffer.
	verticalKernel argAt: 1 putPointer: filterWeightsBuffer! !

!OpenCLBlur12 class methodsFor: 'example' stamp: 'jmv 5/4/2016 16:22'!
blurDisplay
	"
	OpenCLBlur12 blurDisplay
	"
	| blur src dest |
	blur := self new.
	
	dest := ExternalForm extent: Display extent nativeDepthFrom: 32.
	dest allocateSpace.
	src := ExternalForm extent: Display extent nativeDepthFrom: 32.
	src allocateSpace.
	Display displayOn: src.
		
	((1 to: 20), (19 to: 1 by: -1)) do: [:radius |
		| evt startTime queuedTime finishTime |
		startTime := Time millisecondClockValue.
		evt := blur blur: src into: dest radius: radius.
		queuedTime := Time millisecondClockValue - startTime.
		OpenCLEvent waitForEvents: evt.
		finishTime := Time millisecondClockValue - startTime.
		('   QUEUED COMMANDS IN: ', queuedTime printString, 'msecs     ') print.
		('   FINISHED COMMANDS IN: ', finishTime printString, 'msecs     ') print.
		'   QUEUED COMMANDS IN: ', queuedTime printString, 'msecs     ' displayOn: dest at: 20@20.
		'   FINISHED COMMANDS IN: ', finishTime printString, 'msecs     ' displayOn: dest at: 20@40.
		dest displayAt: 0@0.

		(Delay forMilliseconds: 20) wait.
	].
	src displayAt: 0@0.
! !

!OpenCLJuliaSet methodsFor: 'private' stamp: 'jcg 4/28/2010 09:31'!
adjustOriginAndStepSize
	"For now we just hardcode to use the whole domain from (-2,-2) to (2,2)... no zoom/pan."
	params at: 1 put: -2.0.
	params at: 2 put: -2.0.
	params at: 3 put: (4.0 / extent x).
	params at: 4 put: (4.0 / extent y).


	paramsDirty := true.! !

!OpenCLJuliaSet methodsFor: 'private' stamp: 'jmv 7/1/2016 16:54'!
runKernel
	paramsDirty ifTrue: [
		"Would be nice if this were non-blocking..."
		lastEvent := queue
						writeBuffer: paramsBuffer
						from: params
						waitOn: lastEvent.
		paramsDirty := false.
	].

	lastEvent := queue
					runKernel: kernel
					workDim: 2
					globalOffset: #(0 0)
					globalSize: {extent x roundUpTo: 32. extent y roundUpTo: 4}
					localSize: #(32 4)
					waitOn: lastEvent.
	queue
			readImage: workImage
			block: true
			origin: #(0 0 0)
			region: {extent x. extent y. 1}
			rowPitch: outputForm wordsPerLine * 4
			slicePitch: 0
			pointer: outputForm
			waitOn: lastEvent.
	lastEvent := nil! !

!OpenCLJuliaSet methodsFor: 'private' stamp: 'jcg 5/1/2010 01:11'!
runKernelAndDisplay
	self runKernel.
	outputForm displayAt: 0@0.! !

!OpenCLJuliaSet methodsFor: 'private' stamp: 'jmv 8/7/2015 15:16'!
sourceCode
	"Answer the OpenCL source code for the Julia set program."
^'
__constant sampler_t sampler = 	CLK_NORMALIZED_COORDS_TRUE | 
									CLK_FILTER_LINEAR |
									CLK_ADDRESS_CLAMP_TO_EDGE ;

/* 	params[0]	x-origin
	params[1]	y-origin
	params[2]	x-step
	params[3]	y-step
	params[4]	real coefficient
	params[5]	imaginary coefficient
	params[6]	tone-map v-coord (u is computed based on # of iterations)
*/
			
__kernel void
julia (int iterations,
	__constant float* params,
	__read_only image2d_t tone_map,
	__write_only image2d_t dst_image)
{

	if (get_global_id(0) >= get_image_width(dst_image)) return;
	if (get_global_id(1) >= get_image_height(dst_image)) return;
	
	float x = params[0] + (params[2]*(float)get_global_id(0));
	float y = params[1] + (params[3]*(float)get_global_id(1));
	float real = params[4];
	float imaginary = params[5];

	int count = 0;	
	while (count < iterations) {
		if (x*x + y*y > 4.0) break;
		count++;
		
		float new_x = x*x - y*y + real;
		float new_y = 2*x*y + imaginary;
		x = new_x;
		y = new_y;	
	}

	float2 tex_coord = (float2)((float)count / (float)iterations, params[6]);
	write_imagef(	dst_image, 
					(int2)(get_global_id(0), get_global_id(1)), 
					read_imagef(tone_map, sampler, tex_coord));
}'! !

!OpenCLJuliaSet methodsFor: 'private' stamp: 'jcg 4/27/2010 02:06'!
waitForEvent
	"If there's an event pending, wait for it to complete."
	lastEvent ifNotNil: [lastEvent wait].
	lastEvent := nil.! !

!OpenCLJuliaSet methodsFor: 'initialize' stamp: 'jmv 1/5/2016 12:00'!
extent: newExtent
	newExtent = extent ifTrue: [^self "no change"].
	paramsDirty := true.
	extent := newExtent.
	outputForm := Form extent: extent depth: 32.
	
	self waitForEvent.
	workImage ifNotNil: [workImage destroy].
	workImage := context
					newImage2D: CL_MEM_WRITE_ONLY
					format: CLImageFormatStruct bgra8888norm
					width: extent x
					height: extent y
					rowPitch: nil
					hostPointer: nil.
	kernel argAt: 3 putPointer: workImage.
	
	self adjustOriginAndStepSize! !

!OpenCLJuliaSet methodsFor: 'initialize' stamp: 'jmv 9/3/2020 17:12:44'!
initialize
	"We take care so that you can call #initialize again and none of the existing settings will be lost."
	| rememberedExtent |

	lastEvent := nil.	
	super initialize.

	"Set up kernels"
	kernel := program createKernel: 'julia'.

	scale ifNil: [
		"This is the first time #initialize is being run, so we need to initialize all variables.
		 Otherwise, we reuse the existing values, only resetting the OpenCL state."
		scale := 1.0.
		origin := 0.0.
	
		"Would be better to use ExternalData so that we don't have to block when uploading
		 the parameters to the GPU, but I don't know how write 32-bit floats via the FFI."
		params := Float32Array new: 7.
		params at: 5 put: 0.1.
		params at: 6 put: 0.5.
		params at: 7 put: 0.0.
		
		extent := 512@512.
	].

	paramsBuffer := context readWriteBufferOfSize: 7*4. "7 32-bit floats"
	kernel argAt: 1 putPointer: paramsBuffer.

	toneMapForm ifNotNil: [self toneMap: toneMapForm].
	
	rememberedExtent := extent.
	extent := nil. "force recreation of work-image."
	self extent: rememberedExtent.
	
	paramsDirty := true.
	! !

!OpenCLJuliaSet methodsFor: 'initialize' stamp: 'jmv 7/1/2016 16:56'!
toneMap: newToneMapForm
	| |
	
	toneMapForm := newToneMapForm.
	toneMapImage ifNotNil: [
		self waitForEvent.
		toneMapImage destroy.
	].

	"We clamp at a set number of iterations... if we haven't diverged after that long, 
	 we consider the point to be in the set.  We use the number of iterations before
	 diverging to pick a color to shade the pixel.  Therefore, it makes sense to make
	 the number of iterations equal to the width of the tone-map."
	iterations := toneMapForm width.
	kernel argAt: 0 putInt: iterations.

	toneMapImage := context
						newImage2D: CL_MEM_READ_ONLY
						format: CLImageFormatStruct bgra8888norm
						width: toneMapForm width
						height: toneMapForm height
						rowPitch: nil
						hostPointer: nil.
	kernel argAt: 2 putPointer: toneMapImage.
	lastEvent := queue
					writeImage: toneMapImage
					block: true
					origin: #(0 0 0)
					region: {toneMapImage width. toneMapImage height. 1}
					rowPitch: toneMapForm wordsPerLine * 4
					slicePitch: 0
					pointer: toneMapForm
					waitOn: lastEvent.
	paramsDirty := true! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 5/1/2010 01:51'!
imaginary
	"Answer the 'imaginary' parameter of the Julia set."
	^params at: 6! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 5/1/2010 12:47'!
imaginary: newImaginary
	"Set the 'imaginary' parameter of the Julia set."
	| old |
	old := params at: 6.
	params at: 6 put: newImaginary.
	old = (params at: 6) ifTrue: [^self "no change"].
	paramsDirty := true! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 4/28/2010 01:01'!
origin: newOrigin
	self error: 'unimplemented'. "see #adjustOriginAndStepSize"
	origin = newOrigin ifTrue: [^self "no change"].
	origin := newOrigin.
	self adjustOriginAndStepSize.
	! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 5/1/2010 01:46'!
real
	"Answer the 'real' parameter of the Julia set."
	^params at: 5! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 5/1/2010 12:48'!
real: newReal
	"Set the 'real' parameter of the Julia set."
	| old |
	old := params at: 5.
	params at: 5 put: newReal.
	old = (params at: 5) ifTrue: [^self "no change"].
	paramsDirty := true! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 4/28/2010 01:02'!
scale: newScale
	self error: 'unimplemented'. "see #adjustOriginAndStepSize"
	scale = newScale ifTrue: [^self "no change"].
	scale := newScale.
	self adjustOriginAndStepSize.! !

!OpenCLJuliaSet methodsFor: 'parameters' stamp: 'jcg 5/1/2010 01:02'!
toneMapV: texVCoord
	params at: 7 put: texVCoord.
	paramsDirty := true! !

!OpenCLJuliaSet methodsFor: 'accessing' stamp: 'jcg 5/1/2010 01:53'!
isDirty
	"Answer true if parameters have changed and we need to re-render."
	^paramsDirty! !

!OpenCLJuliaSet methodsFor: 'accessing' stamp: 'jcg 4/28/2010 09:29'!
outputForm
	^outputForm! !

!OpenCLJuliaSet methodsFor: 'tone-maps' stamp: 'jmv 8/10/2015 12:13'!
useRainbowToneMap
	"Generate a tone-map that cycles through the color-wheel.."
	| rainbow s |
s _ 32. "original"
s _ 80.
	rainbow := Form extent: s@s depth: 32.
	1 to: s-2 do: [:i |
		0 to: s-1 do: [:j |
			| hue bright |
			hue := ((j*11.25*32/s) + (i *11.25*32/s)) \\ 360.0.
			bright := (s-2 - i) / (s-2*2.0) + 0.35.
			rainbow colorAt: i@j put: (Color h: hue s: 1.0 v: bright).
		].
	].
	0 to: s-1 do: [:j |
		rainbow colorAt: 0@j put: (Color transparent).
		rainbow colorAt: s-1@j put: (Color transparent).
	].

	self toneMap: rainbow.! !

!OpenCLJuliaSet class methodsFor: 'examples' stamp: 'jmv 8/7/2015 15:03'!
example1
	"
	OpenCLJuliaSet example1
	"
	| clExample |

	clExample := OpenCLJuliaSet new.
	clExample extent: 1024@1024.
	clExample useRainbowToneMap.
	
	(ImageMorph new image:  clExample outputForm) openInHand.	
			
	0 to: 360 by: 2 do: [:i |
		| rads |
		rads := i degreesToRadians.
		clExample real: rads sin / 2.0*1.3.
		clExample imaginary: rads cos / 2.0*1.1.	
		clExample runKernel.
		clExample outputForm display.
	"	Display forceToScreen: (0@0 corner: 1024@1024)."
	].


	
! !

!OpenCLFill256 methodsFor: 'initialize' stamp: 'jmv 7/1/2019 17:51:13'!
initialize
	device := OpenCLPlatform preferredDevice.
	context := OpenCLContext newForDevices: {device}.
	queue := context commandQueueOn: device.
	program := context programWithSource: self sourceCode.
	program build.
	buffer := context readWriteBufferOfSize: 256*4.  "256 integers"
	kernel1d := program createKernel: 'fill256_1d'.
	kernel1d argAt: 0 putPointer: buffer.
	kernel2d := program createKernel: 'fill256_2d'.
	kernel2d argAt: 0 putPointer: buffer! !

!OpenCLFill256 methodsFor: 'actions' stamp: 'jcg 2/16/2010 10:57'!
run1d
	| evt output |
	evt := queue 
			runKernel: kernel1d
			workDim: 1
			globalOffset: #(0)
			globalSize: #(256)
			localSize: #(16)
			waitOn: nil.
	output := IntegerArray new: 256.
	queue readBuffer: buffer into: output waitOn: evt.
	^output! !

!OpenCLFill256 methodsFor: 'actions' stamp: 'jcg 2/16/2010 11:00'!
run2d
	| evt output |
	evt := queue 
			runKernel: kernel2d
			workDim: 2
			globalOffset: #(0 0)
			globalSize: #(16 16)
			localSize: #(4 4)
			waitOn: nil.
	output := IntegerArray new: 256.
	queue readBuffer: buffer into: output waitOn: evt.
	^output! !

!OpenCLFill256 methodsFor: 'private' stamp: 'jmv 8/7/2015 15:12'!
sourceCode
	"Answer the OpenCL source code for this example."
^'
	__kernel void
	fill256_1d(__global int* target)
	{
		int index = get_global_id(0);
		target[index] = index;
	}
	
	__kernel void
	fill256_2d(__global int* target)
	{
		int x = get_global_id(0);
		int y = get_global_id(1);
		int index = x*16 + y;
		target[index] = index;
	}'! !

!Object methodsFor: '*OpenCL' stamp: 'jcg 2/18/2010 03:08'!
asOpenCLArg
	"Default is to pass self as arg to FFI call."
	^self! !

!False methodsFor: '*OpenCL' stamp: 'jcg 2/16/2010 01:24'!
asOpenCLBoolean
	^0 "CLFalse"
	! !

!True methodsFor: '*OpenCL' stamp: 'jcg 2/16/2010 01:24'!
asOpenCLBoolean
	^1 "CLTrue"
	! !

!Float methodsFor: '*OpenCL' stamp: 'jmv 1/5/2016 11:12'!
asOpenCLArg
	"To be used with OpenCL arguments declared as 'double'"
	^Float64Array with: self! !

!SmallInteger methodsFor: '*OpenCL' stamp: 'jmv 1/5/2016 11:08'!
asOpenCLArg
	"Assume we fit in 32 bits"
	^IntegerArray with: self! !

!SmallInteger methodsFor: '*OpenCL' stamp: 'jcg 2/16/2010 01:23'!
asOpenCLBoolean
	"Assume that we're already CLTrue or CLFalse (if not, there will be an error anyway)."
	^self! !

!Form methodsFor: '*OpenCL' stamp: 'jcg 2/18/2010 03:05'!
asOpenCLArg
	"Pass pointer to our bitmap."
	^bits! !

!ExternalForm methodsFor: '*OpenCL' stamp: 'jcg 6/4/2010 02:20'!
asOpenCLArg
	"Pass pointer to our pixels."
	^pointer ifNil: [self error: 'pointer is nil'].! !

!ExternalObject methodsFor: '*OpenCL' stamp: 'jcg 2/18/2010 03:02'!
asOpenCLArg
	"Pass pointer to external memory (or potentially, a ByteArray) as an argument to a kernel or other function."
	^self getHandle! !
OpenCLConstants initialize!
CLBufferRegionStruct initialize!
CLImageDescStruct initialize!
OpenCLObject initialize!
OpenCLPrimitiveInterface initialize!
OpenCLRefCounter initialize!
