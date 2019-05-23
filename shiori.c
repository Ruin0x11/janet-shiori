/**
 * @file
 * @brief SHIORI/3.x DLLインターフェース
 * @author Narazaka: http://narazaka.net/
 *
 * (C) 2014 Narazaka : Licensed under The MIT License - http://narazaka.net/license/MIT?2014
 **/
#include <stdlib.h>
#include <string.h>
#include "bool.h"
#include "shiori.h"

#include "hardwarebp.c"
#include "strutl.c"
#include "janet/dist/janet.c"
#include "cshiori.c"
#include "shiori_events.c"

FILE* out = NULL;
int ind = -1;
int bindd = -1;

SHIORI_EXPORT bool SHIORI_CALL load(const MEMORY_HANDLE h,long len){
	char* str = (char*)malloc(sizeof(char) * (len + 1));
	strncpy(str, (const char*)h, len);
	str[len] = '\0';
	SHIORI_FREE(h);

        out = fopen("shiori-native.txt", "w");
        fputs("*L:\n", out);

        bool result = cshiori_load(str, shiori_load);

        //ind = HardwareBreakpoint_Set(&janet_vm_cache_capacity, sizeof(janet_vm_cache_capacity), HWBP_WRITE);

        //bindd = HardwareBreakpoint_Set(&janet_vm_cache, sizeof(janet_vm_cache), HWBP_WRITE);

        free(str);
        return result;
}

#define SHIORI_LINES_BUFFER_STEP 10
SHIORI_EXPORT MEMORY_HANDLE SHIORI_CALL request(const MEMORY_HANDLE h,long *len){
	char* str = (char*)malloc(sizeof(char) * (*len + 1));

	strncpy(str, (const char*)h, *len);
	str[*len] = '\0';

	SHIORI_FREE(h);

#if defined(WIN32)||defined(_WIN32)||defined(_Windows)||defined(__CYGWIN__)
        char* str_crlf = str;
        str = crlftolf(str_crlf);
        free(str_crlf);
#endif
        fputs("*S:\n", out);
        fputs(str, out);

        char* resstr = cshiori_requestb(str, shiori_requestb);
        if (resstr == NULL) {
            resstr = cshiori_shiori_response_build_internal_server_error();
        }

        // fputs(resstr, out);

	*len = strlen(resstr);
	MEMORY_HANDLE reth = (MEMORY_HANDLE)SHIORI_MALLOC(*len);
	memcpy(reth, resstr, *len);

        free(resstr);

	return reth;
}

SHIORI_EXPORT bool SHIORI_CALL unload(void){
    // HardwareBreakpoint_Clear(bindd);
    // HardwareBreakpoint_Clear(ind);
    ind = -1;
        fputs("*U:\n", out);
    fclose(out);
	return cshiori_unload(shiori_unload);
}
