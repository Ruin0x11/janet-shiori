#include <string.h>
#include <time.h>
#include <stdlib.h>
#include "janet/janet.h"

JanetTable* env = NULL;

int run_janet_script(const char* path)
{
    FILE *f = fopen(path, "rb");
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *string = (char*)malloc(fsize + 1);
    fread(string, 1, fsize, f);
    fclose(f);

    string[fsize] = 0;

    int status = janet_dobytes(env, string, fsize, path, NULL);

    free(string);

    return status;
}

bool shiori_load(const char* dirpath){
    janet_init();

    JanetTable* replacements = janet_table(0);
    env = janet_core_env(replacements);
    janet_def(env, "shiori-dirpath", janet_cstringv(dirpath), "Startup path provided by SHIORI.");

    return run_janet_script("init.janet");
}

bool shiori_unload(void){
    janet_deinit();
    env = NULL;

    return true;
}

char* copy_string(const char* source){
	char* dest = (char*)malloc(strlen(source) + 1);
	strcpy(dest, source);
	return dest;
}

struct cshiori_response_message* shiori_request(struct cshiori_request_message* req, struct cshiori_response_message* res){
	static int aitalk_count = 0;
	res->version = copy_string("3.0");
	res->sender = copy_string("cshiori");
	res->charset = copy_string("UTF-8");
	if(req->method == NOTIFY){
		res->status_code = 204;
		return res;
	}
	if(req->id == NULL){
		return NULL;
	}
	if(0 == strcmp(req->id, "version")){
		res->status_code = 200;
		res->value = copy_string("0.0.1");
	}else if(0 == strcmp(req->id, "name")){
		res->status_code = 200;
		res->value = copy_string("cshiori");
	}else if(0 == strcmp(req->id, "craftman")){
		res->status_code = 200;
		res->value = copy_string("Narazaka");
	}else if(0 == strcmp(req->id, "craftmanw")){
		res->status_code = 200;
		res->value = copy_string("奈良阪某");
	}else if(0 == strcmp(req->id, "OnFirstBoot")){
		res->status_code = 200;
		res->value = copy_string("\\h\\s[0]初回起動。\\e");
	}else if(0 == strcmp(req->id, "OnBoot")){
		res->status_code = 200;
		res->value = copy_string("\\h\\s[0]起動。\\e");
	}else if(0 == strcmp(req->id, "OnGhostChanged")){
		res->status_code = 200;
		res->value = copy_string("\\h\\s[0]交代。\\e");
	}else if(0 == strcmp(req->id, "OnGhostChanging")){
		res->status_code = 200;
		res->value = copy_string("\\h\\s[0]交代。\\e");
	}else if(0 == strcmp(req->id, "OnSecondChange")){
		aitalk_count ++;
		if(aitalk_count == 90){
			static char* const aitalks[] = {
				"\\h\\s[0]C言語って、\\w4胸キュン？\\e",
				"\\h\\s[5]MiyoJSもよろしく。\\e",
				"\\h\\s[0]C++だとconst char*なリテラルをchar*に入れるなっていう警告が出るけど、\\w9\\s[8]まあコンパイルとおるからいいよね？\\e",
				"\\h\\s[0]古いVC++ではstdbool.hがなくてコンパイルできないから、\\w9\\s[8]マクロでbool定めてるっていう……。\\e",
				"\\h\\s[6]あとの実装はキミしだい。\\e"
			};
			static const size_t aitalks_length = 5;
			aitalk_count = 0;
			srand((unsigned)time(NULL));
			res->status_code = 200;
			res->value = copy_string(aitalks[rand() % aitalks_length]);
		}else{
			res->status_code = 204;
		}
	}else if(0 == strcmp(req->id, "OnClose")){
		res->status_code = 200;
		res->value = copy_string("\\h\\s[0]終了。\\w9\\-");
	}else{
		return NULL;
	}
	return res;
}

char* shiori_requestb(const char* str){
    Janet request;
    JanetBindingType stat = janet_resolve(env, janet_csymbol("shiori/request"), &request);
    if (stat != JANET_BINDING_DEF) {
        return NULL;
    }

    if (janet_type(request) != JANET_FUNCTION) {
        return NULL;
    }

    JanetFunction* request_func = janet_unwrap_function(request);

    Janet sj = janet_wrap_string(str);
    Janet arg = janet_wrap_tuple(&sj);
    Janet result;
    JanetFiber* fiber = NULL;

    int lock = janet_gclock();
    JanetSignal sig = janet_pcall(request_func,
                                  1,
                                  &arg,
                                  &result,
                                  &fiber);
    janet_gcunlock(lock);

    if (sig != JANET_SIGNAL_OK) {
        return NULL;
    }

    if(janet_type(result) != JANET_STRING) {
        return NULL;
    }

    const uint8_t* s = janet_unwrap_string(result);
    char* st = (char*)malloc(sizeof(char) * janet_string_length(s) + 1);
    strcpy(st, s);

    return st;
}
