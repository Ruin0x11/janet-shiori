/**
 * @file
 * @brief SHIORI/3.x SHIOLINKインターフェース
 * @author Narazaka: http://narazaka.net/
 *
 * (C) 2014 Narazaka : Licensed under The MIT License - http://narazaka.net/license/MIT?2014
 **/
#include <stdio.h>
#include <stdlib.h>

#include "strutl.c"
#include "cshiori.c"
#include "cshiolink.c"
#include "shiori_events.c"

int main(void){
    FILE *f = fopen("cmd.txt", "rb");
        cshiolink_mainloop(f, stdout, shiori_load, shiori_requestb, shiori_unload);
	return 0;
}
