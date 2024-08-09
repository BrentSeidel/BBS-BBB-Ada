/*
 *  This is trivial.  I don't care if you copy this or not.
 */
#include <errno.h>
int get_errno()
{
  return errno;
}

void reset_errno()
{
  errno = 0;
}
