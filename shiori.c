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

#include "janet/janet.c"
#include "cshiori.c"
#include "shiori_events.c"

SHIORI_EXPORT bool SHIORI_CALL load(const MEMORY_HANDLE h,long len){
	char* str = (char*)malloc(sizeof(char) * (len + 1));
	strncpy(str, (const char*)h, len);
	str[len] = '\0';
	SHIORI_FREE(h);

	bool result = cshiori_load(str, shiori_load);

        free(str);
        return result;
}

#define SHIORI_LINES_BUFFER_STEP 10
SHIORI_EXPORT MEMORY_HANDLE SHIORI_CALL request(const MEMORY_HANDLE h,long *len){
	char* str = (char*)malloc(sizeof(char) * (*len + 1));

	strncpy(str, (const char*)h, *len);
	str[*len] = '\0';

	SHIORI_FREE(h);

	char* resstr = cshiori_requestb(str, shiori_requestb);
        if (resstr == NULL) {
            resstr = cshiori_shiori_response_build_internal_server_error();
        }

	*len = strlen(resstr);
	MEMORY_HANDLE reth = (MEMORY_HANDLE)SHIORI_MALLOC(*len);
	memcpy(reth, resstr, *len);

        free(resstr);

	return reth;
}

SHIORI_EXPORT bool SHIORI_CALL unload(void){
	return cshiori_unload(shiori_unload);
}
