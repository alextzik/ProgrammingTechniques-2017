#ifndef __SCREEN_H__
#define __SCREEN_H__

#include <sys/ioctl.h>
#include <iostream>
#include <stdlib.h>

namespace tui {
    
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
    
    using std::cout;
    using std::cin;
    
    typedef unsigned char byte;
    
    struct point {
        char _ch;
        byte _color_code;
        byte _bg_color_code;
        bool _bright;
    private:
        static const int default_color = 9;
    public:
        point()
        : _ch(' '), _color_code(default_color), _bg_color_code(default_color), _bright(true) {
        }
        point(char ch, byte color_code)
        : _ch(ch), _color_code(color_code), _bg_color_code(default_color), _bright(true) {
        }
        point(char ch, byte color_code, byte bg_color_code, bool bright)
        : _ch(ch), _color_code(color_code), _bg_color_code(bg_color_code), _bright(bright) {
        }
    };
    
    class screen {
    private:
        point *_buffer;
        int _ncols;
        int _nrows;
    public:
        screen(int cols, int rows)
        : _buffer(0) {
            init(cols, rows);
        }
        ~screen() {
            dispose();
        }
        int ncols() const {
            return _ncols;
        }
        int nrows() const {
            return _nrows;
        }
        void clear() const {
            cout << "\033[2J";
        }
        void set_rect(int x0, int y0, int x1, int y1, const point &pt) {
            for(int irow = y0; irow <= y1; irow++) {
                set_point(x0, irow, pt);
                set_point(x1, irow, pt);
            }
            for(int icol = x0; icol <= x1; icol++) {
                set_point(icol, y0, pt);
                set_point(icol, y1, pt);
            }
        }
        void set_circle(int x, int y, int rad, const point &pt) {
            int icol = rad;
            int irow = 0;
            int error = 1 - icol;
            while(icol >= irow) {
                set_point(icol + x, irow + y, pt);
                set_point(irow + x, icol + y, pt);
                set_point(-icol + x, irow + y, pt);
                set_point(-irow + x, icol + y, pt);
                set_point(-icol + x, -irow + y, pt);
                set_point(-irow + x, -icol + y, pt);
                set_point(icol + x, -irow + y, pt);
                set_point(irow + x, -icol + y, pt);
                
                irow++;
                
                if(error < 0) {
                    error += 2 * irow + 1;
                } else {
                    icol--;
                    error += 2 * (irow - icol + 1);
                }
            }
        }
        void render() {
            ensure_initialized();
            int last_color_code = -1;
            int last_bg_color_code = -1;
            bool last_bright = false;
            for(int irow = 0; irow < nrows(); irow++) {
                for(int icol = 0; icol < ncols(); icol++) {
                    point &pt = _buffer[ncols() * irow + icol];
                    
                    if(pt._bright != last_bright) {
                        set_color(pt._bright ? 1 : 22);
                        last_bright = pt._bright;
                    }
                    
                    if(pt._color_code != last_color_code) {
                        set_color(30 + pt._color_code);
                        last_color_code = pt._color_code;
                    }
                    
                    if(pt._bg_color_code != last_bg_color_code) {
                        set_color(40 + pt._bg_color_code);
                        last_bg_color_code = pt._bg_color_code;
                    }
                    
                    cout << pt._ch;
                }
                cout << "\n";
            }
            set_color(22);
            set_color(39);
            set_color(49);
        }
        
    private:
        void set_point(int icol, int irow, const point &pt) {
            ensure_initialized();
            if(coords_invalid(irow, icol))
                return;
            _buffer[ncols() * irow + icol] = pt;
        }
        void set_color(int color_code) const {
            cout << "\033[" << color_code << "m";
        }
        bool coords_invalid(int irow, int icol) {
            ensure_initialized();
            return irow < 0 || icol < 0 || irow >= nrows() || icol >= ncols();
        }
        void ensure_initialized() {
            if(!_buffer)
                init(80,30);
        }
        void init(int cols, int rows) {
            dispose();
            update_size(cols, rows);
            _buffer = new point[nrows() * ncols()];
        }
        void dispose() {
            if(_buffer)
                delete [] _buffer;
        }
        void update_size(int cols, int rows) {
#ifdef TIOCGSIZE
            struct ttysize ts;
            ioctl(STDIN_FILENO, TIOCGSIZE, &ts);
            _ncols = cols;
            _nrows = rows;
#elif TIOCGWINSZ
            struct winsize ts;
            ioctl(STDIN_FILENO, TIOCGWINSZ, &ts);
            _ncols = cols;
            _nrows = rows;
#else
            _ncols = cols;
            _nrows = rows;
#endif
            _nrows -= 1; // $>
        }
    };
    
}

#endif // __SCREEN_H__

