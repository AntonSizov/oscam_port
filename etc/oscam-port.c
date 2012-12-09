#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "cscrypt/des.h"

#define bool unsigned int
#define true (1)
#define false (0)
#define ERL_MSG_HEADER_SIZE ((size_t)2)
#define ERL_MSG_MAX_SIZE ((size_t)65536)
#define byte_t unsigned char

#define IDX_DES_LOGIN_KEY_GET 1

int write_exact(byte_t* buff, int len) {
    int i; int wrote =  0 ;
    do {
        if ( (i = write(1/*STDOUT*/, buff + wrote, len - wrote)) <=  0  ) {
            return i;
        }
        wrote += i;
    } while (wrote < len);
    return len;
}

int write_cmd(byte_t* buff, int len) {
    byte_t li;
    li = (len >> 8) & 0xff;
    write_exact(&li, 1);
    li = len & 0xff;
    write_exact(&li, 1);
    return write_exact(buff, len);
}

static void process(byte_t *payload, int length) {
    int fun_idx;
    fun_idx = payload[0];

	if(fun_idx == IDX_DES_LOGIN_KEY_GET) {
	    byte_t key[16];
	    des_login_key_get(&payload[1], &payload[15], 14, key);
		write_cmd(key, 16);
	}
}


static size_t packet_size(byte_t buff[ERL_MSG_HEADER_SIZE]) {
    int len;
	len = (buff[0] << 8) | buff[1];
	return len;
}

int main(int argc, char** argv) {
	bool bShutdown = false;
	fprintf(stderr, "Entering the loop\n");
	while (! bShutdown) {
		fprintf(stderr, "Loop begin\n");
		byte_t packet_size_buff[ERL_MSG_HEADER_SIZE];
		byte_t packet_payload[ERL_MSG_MAX_SIZE];
		ssize_t bytes_read = 0;
		bytes_read = read(fileno(stdin), (void*) packet_size_buff, ERL_MSG_HEADER_SIZE);
		fprintf(stderr, "Bytes read: %d\n", bytes_read);

		if ( bytes_read > 0 ) {
			size_t bytes_to_read = packet_size(packet_size_buff);
			fprintf(stderr, "Packet size: %d\n", bytes_to_read);

			bytes_read = read(fileno(stdin), (void*) packet_payload, bytes_to_read);
			if ( bytes_read != bytes_to_read ) {
				fprintf(stderr, "wrong bytes_read %d. Expected: %d\n. Errno: %s\n", bytes_read, bytes_to_read, strerror(errno));
				bShutdown = true;
			}
			else {
			  process(packet_payload, bytes_to_read);
			}
		}
		else {
			fprintf(stderr, "Error: %s\n", strerror(errno));
			bShutdown = true;
		}
		fprintf(stderr, "Loop end\n");
	}
	fprintf(stderr, "Left the loop\n");

	return 0;
}
