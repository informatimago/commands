#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>


int main(void){
    struct stat s;
#define p(type)  printf("%-20s %4lu\n",#type,sizeof(type))
    p(dev_t);
    p(ino_t);
    p(mode_t);
    p(nlink_t);
    p(uid_t);
    p(gid_t);
    p(struct timespec);
    p(off_t);
    p(quad_t);
    p(u_long);
    p(blkcnt_t);
    p(blksize_t);
    p(uint32_t);
    p(int32_t);
    p(int64_t);
	p(__darwin_time_t);
	p(long);           

    return 0;
}
