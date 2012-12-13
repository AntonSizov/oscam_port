#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "cscrypt/cscrypt.h"

#define CWS_NETMSGSIZE 362

#define IDX_DES_LOGIN_KEY_GET 1
#define MD5_CRYPT 2
#define DES_ENCRYPT 3
#define DES_DECRYPT 4

typedef unsigned char byte;

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int main(void) {
  int fn, len;
  byte buf[65536];

  while ((len = read_cmd(buf)) > 0) {
    fn = buf[0];

    if (fn == IDX_DES_LOGIN_KEY_GET) {
      byte key[16];

      des_login_key_get(&buf[1], &buf[15], 14, key);
	  write_cmd(key, 16);

    } else if (fn == MD5_CRYPT) {
	  byte passwdcrypt[120];

      __md5_crypt((char *)&buf[1], "$1$abcdefgh$", (char *)passwdcrypt);
	  write_cmd(passwdcrypt, strlen((char *)passwdcrypt));

    } else if (fn == DES_ENCRYPT) {
	  byte desbuf[CWS_NETMSGSIZE];
	  int encrypted_len, msg_len;

	  msg_len = len-17;
	  memcpy(desbuf, &buf[18], msg_len);
	  encrypted_len = des_encrypt(desbuf, msg_len , &buf[1]);
	  fprintf(stderr, "encrypted len: %d, input msg len: %d\n", encrypted_len, msg_len);
	  write_cmd(desbuf, encrypted_len);

	} else if (fn == DES_DECRYPT) {

	  byte desbuf[CWS_NETMSGSIZE];
	  int decrypted_len, input_len;

	  input_len = len-17;
	  fprintf(stderr, "des_decrypt input len: %d\n", input_len);
	  memcpy(desbuf, &buf[18], input_len);
	  if ((decrypted_len = des_decrypt(desbuf, input_len, &buf[1])) < 0)
		fprintf(stderr, "des_decrypt error (%d)\n", decrypted_len);
		return -1;
	  fprintf(stderr, "decrypted len: %d, input len: %d\n", decrypted_len, input_len);
	  write_cmd(desbuf, decrypted_len);

	}
  }
  return 0;
}
