#include <gdk-pixbuf/gdk-pixbuf.h>
// gcc pixbuf_vuln_poc.c -o pixbuf_vuln_poc  `pkg-config --libs --cflags gdk-pixbuf-2.0`

int main(int argc, char **argv) {
    GdkPixbuf* buf;
    int size = 32;
    GError* err = NULL;

    buf =  gdk_pixbuf_new_from_file_at_size(argv[1], size, size, &err);

    if (err)
      printf ("Gerror: %s\n", err->message);

    g_object_unref(buf);
    return 0;
}
