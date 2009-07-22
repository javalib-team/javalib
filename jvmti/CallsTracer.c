#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <jvmti.h>

/* ------------------------------------------------------------------- */
static jvmtiEnv *jvmti = NULL;
static jvmtiCapabilities capa;

static const char *jvmti_outvar = "JVMTI_OUT";
static char *jvmti_output = "./callstrace.jvmti";

/* Global agent data structure */

typedef struct {
  /* JVMTI Environment */
  jvmtiEnv      *jvmti;
  jboolean       vm_is_started;
  /* Data access Lock */
  jrawMonitorID  lock;
} GlobalAgentData;

static GlobalAgentData *gdata;

/* Every JVMTI interface returns an error code, which should be checked
 *   to avoid any cascading errors down the line.
 *   The interface GetErrorName() returns the actual enumeration constant
 *   name, making the error messages much easier to understand.
 */
static void
check_jvmti_error(jvmtiEnv *jvmti, jvmtiError errnum, const char *str)
{
  if ( errnum != JVMTI_ERROR_NONE ) {
    char       *errnum_str;
    
    errnum_str = NULL;
    (void)(*jvmti)->GetErrorName(jvmti, errnum, &errnum_str);
    
    printf("ERROR: JVMTI: %d(%s): %s\n", errnum, (errnum_str==NULL?"Unknown":errnum_str), (str==NULL?"":str));
  }
}

/* Enter a critical section by doing a JVMTI Raw Monitor Enter */
static void
enter_critical_section(jvmtiEnv *jvmti)
{
  jvmtiError error;
  
  error = (*jvmti)->RawMonitorEnter(jvmti, gdata->lock);
  check_jvmti_error(jvmti, error, "Cannot enter with raw monitor");
}

/* Exit a critical section by doing a JVMTI Raw Monitor Exit */
static void
exit_critical_section(jvmtiEnv *jvmti)
{
  jvmtiError error;
  
  error = (*jvmti)->RawMonitorExit(jvmti, gdata->lock);
  check_jvmti_error(jvmti, error, "Cannot exit with raw monitor");
}

static void print_spaces(jint n){
  int i = 0;
  for (i=0; i<n; i++){
    printf(" ");
  }
}

static void JNICALL callbackVMObjectAlloc(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jobject object, jclass object_klass, jlong size) {
  char *className;
  jvmtiThreadInfo thread_info;
  jvmtiError err;
  FILE *file;

  err = (*jvmti)->GetClassSignature(jvmti, object_klass, &className, NULL);
  err = (*jvmti)->GetThreadInfo(jvmti, NULL, &thread_info);

  if (err == JVMTI_ERROR_NONE && className != NULL
      && strcmp(thread_info.name,"DestroyJavaVM")) {
    file = fopen(jvmti_output,"a");
    fprintf(file, "VMAlloc{thread=\"%s\" class=\"%s\"}\n", thread_info.name, className);
    fclose(file);
  }

  err = (*jvmti)->Deallocate(jvmti, (unsigned)className);
  err = (*jvmti)->Deallocate(jvmti, (unsigned)thread_info.name);
}

static JNICALL callbackMethodEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method){
  char *methodName;
  char *methodSignature;
  char *declaringClassName;
  jclass declaring_class;
  jvmtiThreadInfo thread_info;
  /* jint frame_count = 0; */
  jboolean is_native = 0;
  jvmtiError err;
  char *type = "Java";

  err = (*jvmti)->GetMethodName(jvmti, method, &methodName, &methodSignature, NULL);
  /* err = (*jvmti)->GetFrameCount(jvmti, NULL, &frame_count); */
  err = (*jvmti)->GetMethodDeclaringClass(jvmti, method, &declaring_class);
  err = (*jvmti)->GetClassSignature(jvmti, declaring_class, &declaringClassName, NULL);
  err = (*jvmti)->GetThreadInfo(jvmti, NULL, &thread_info);
  err = (*jvmti)->IsMethodNative(jvmti, method, &is_native);

  if (err == JVMTI_ERROR_NONE && strcmp(thread_info.name,"DestroyJavaVM")){
    if (is_native > 0){
      type = "Native";
    }
    FILE *file;
    file = fopen(jvmti_output,"a");
    fprintf(file, "MethodEntry{thread=\"%s\" type=\"%s\" class=\"%s\" name=\"%s\" signature=\"%s\"}\n", thread_info.name,
	    type, declaringClassName, methodName, methodSignature);
    fclose(file);
  }
  
  err = (*jvmti)->Deallocate(jvmti, (unsigned)methodSignature);
  err = (*jvmti)->Deallocate(jvmti, (unsigned)methodName);
  err = (*jvmti)->Deallocate(jvmti, (unsigned)declaringClassName);
  err = (*jvmti)->Deallocate(jvmti, (unsigned)thread_info.name);
}

static JNICALL callbackMethodExit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method){
  /* char *methodName; */
  /* char *methodSignature; */
  /* char *declaringClassName; */
  /* jclass declaring_class; */
  jvmtiThreadInfo thread_info;
  /* jint frame_count = 0; */
  jvmtiError err;
  
  /* err = (*jvmti)->GetMethodName(jvmti, method, &methodName, &methodSignature, NULL); */
  /* err = (*jvmti)->GetFrameCount(jvmti, NULL, &frame_count); */
  /* err = (*jvmti)->GetMethodDeclaringClass(jvmti, method, &declaring_class); */
  /* err = (*jvmti)->GetClassSignature(jvmti, declaring_class, &declaringClassName, NULL);  */
  err = (*jvmti)->GetThreadInfo(jvmti, NULL, &thread_info);
  if (err == JVMTI_ERROR_NONE && strcmp(thread_info.name,"DestroyJavaVM")){
    FILE *file;
    file = fopen(jvmti_output,"a");
    fprintf(file, "MethodExit{thread=\"%s\"}\n", thread_info.name);
    fclose(file);
  }
  /* err = (*jvmti)->Deallocate(jvmti, (unsigned)methodSignature); */
  /* err = (*jvmti)->Deallocate(jvmti, (unsigned)methodName); */
  /* err = (*jvmti)->Deallocate(jvmti, (unsigned)declaringClassName); */
  err = (*jvmti)->Deallocate(jvmti, (unsigned)thread_info.name);
}

JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *jvm, char *options, void *reserved)
{
  static GlobalAgentData data;
  jvmtiError error;
  jint res;
  jvmtiEventCallbacks callbacks;
  
  char *out = getenv(jvmti_outvar);
  if (out != NULL){
    jvmti_output = out;
  }

  /* We create the output file */
  FILE *file;
  file = fopen(jvmti_output,"w");
  fclose(file);

  /* Setup initial global agent data area
   *   Use of static/extern data should be handled carefully here.
   *   We need to make sure that we are able to cleanup after ourselves
   *     so anything allocated in this library needs to be freed in
   *     the Agent_OnUnload() function.
   */
  (void)memset((void*)&data, 0, sizeof(data));
  gdata = &data;
  
  /*  We need to first get the jvmtiEnv* or JVMTI environment */
  
  res = (*jvm)->GetEnv(jvm, (void **) &jvmti, JVMTI_VERSION_1_0);
  
  if (res != JNI_OK || jvmti == NULL) {
    /* This means that the VM was unable to obtain this version of the
     *   JVMTI interface, this is a fatal error.
     */
    printf("ERROR: Unable to access JVMTI Version 1 (0x%x),"
	   " is your J2SE a 1.5 or newer version?"
	   " JNIEnv's GetEnv() returned %d\n",
	   JVMTI_VERSION_1, res);
    
  }
  
  /* Here we save the jvmtiEnv* for Agent_OnUnload(). */
  gdata->jvmti = jvmti;
  
  (void)memset(&capa, 0, sizeof(jvmtiCapabilities));
  capa.can_generate_method_entry_events = 1;
  capa.can_generate_method_exit_events = 1;
  capa.can_generate_vm_object_alloc_events = 1;
  
  error = (*jvmti)->AddCapabilities(jvmti, &capa);
  check_jvmti_error(jvmti, error, "Unable to get necessary JVMTI capabilities.");
  
  
  (void)memset(&callbacks, 0, sizeof(callbacks));
  callbacks.VMObjectAlloc = &callbackVMObjectAlloc;/* JVMTI_EVENT_VM_OBJECT_ALLOC */
  callbacks.MethodEntry = &callbackMethodEntry; /* JVMTI_EVENT_METHOD_ENTRY */
  callbacks.MethodExit = &callbackMethodExit; /* JVMTI_EVENT_METHOD_EXIT */
  
  error = (*jvmti)->SetEventCallbacks(jvmti, &callbacks, (jint)sizeof(callbacks));
  check_jvmti_error(jvmti, error, "Cannot set jvmti callbacks");
  
  /* At first the only initial events we are interested in are VM
   *   initialization, VM death, and Class File Loads.
   *   Once the VM is initialized we will request more events.
   */
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE, JVMTI_EVENT_METHOD_ENTRY, (jthread)NULL);
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE, JVMTI_EVENT_METHOD_EXIT, (jthread)NULL);
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE, JVMTI_EVENT_VM_OBJECT_ALLOC, (jthread)NULL);
  check_jvmti_error(jvmti, error, "Cannot set event notification");
  
  
  /* Here we create a raw monitor for our use in this agent to
   *   protect critical sections of code.
   */
  error = (*jvmti)->CreateRawMonitor(jvmti, "agent data", &(gdata->lock));
  check_jvmti_error(jvmti, error, "Cannot create raw monitor");
  
  /* We return JNI_OK to signify success */
  return JNI_OK;
}


/* Agent_OnUnload: This is called immediately before the shared library is
 *   unloaded. This is the last code executed.
 */
JNIEXPORT void JNICALL
Agent_OnUnload(JavaVM *vm)
{
  /* Make sure all malloc/calloc/strdup space is freed */
  
}
