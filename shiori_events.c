#include <string.h>
#include <time.h>
#include <stdlib.h>

bool shiori_load(const char* dirpath){
	return true;
}

bool shiori_unload(void){
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
	res->charset = copy_string("Shift_JIS");
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
