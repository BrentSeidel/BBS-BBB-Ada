#include <errno.h>
int get_errno()
{
  return errno;
}

void reset_errno()
{
  errno = 0;
}
