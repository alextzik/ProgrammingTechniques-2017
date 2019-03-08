#include <sys/ioctl.h>
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <utility>
#include "ps6.h"

using std::vector;
using std::pair;

// ############################################################
//                     YOUR IMPLEMENTATION                   //
// ############################################################

namespace tui {
    
    class color {
    private:
        byte code=0;
        color(byte code) {
            this->code = code;
        };
        
        
    public:
        byte get_code() const {
            return code;
        }
        
        static const color black;
        static const color red;
        static const color green;
        static const color yellow;
        static const color blue;
        static const color magenta;
        static const color white;
        static const color system_default;
    };
    const color color::black(0);
    const color color::red(1);
    const color color::green(2);
    const color color::yellow(3);
    const color color::blue(4);
    const color color::magenta(5);
    const color color::white(7);
    const color color::system_default(9);
    
    
    
    class pen {
    private:
        char _c;
        color _foreground;
        color _background;
        bool _bright;
        
    public:
        pen(color color_code, color bg_color_code, char ch, bool bright)
        :_foreground(color_code), _background(bg_color_code), _c(ch), _bright(bright) {
        }
        point pen_to_point() const {
            point p;
            p._color_code=_foreground.get_code();
            p._bright=_bright;
            p._ch=_c;
            p._bg_color_code=_background.get_code();
            return p;
        }
    };
    
    class shape {
    private:
        
    public:
        virtual void draw(screen &scr, const pen &p) const=0;
        virtual ~shape(){};
        
    };
    
    // all correct
    
    
    class rectangle : public shape {
    private:
        int _width;
        int _length;
        int _x0;
        int _y0;
        int _x1;
        int _y1;
    public:
        rectangle(int x0, int y0, int width, int length)
        :_x0(x0),_y0(y0),_width(width),_length(length){
        }
        ~rectangle(){};
        int getwidth(){
            return _width;
        }
        int getlength(){
            return _length;
        }
        int get_x0(){
            return _x0;
        }
        int get_y0(){
            return _y0;
        }
        int get_x1(){
            return _x1;
        }
        int get_y1(){
            return _y1;
        }
        
        void draw(screen &scr, const pen &p) const{
            scr.set_rect(_x0, _y0, _x0+_width, _y0+_length, p.pen_to_point());
        }
        
    };
    
    class circle : public shape {
    private:
        int _x;
        int _y;
        int _rad;
    public:
        circle(int x, int y, int rad)
        :_x(x),_y(y),_rad(rad)
        {}
        ~circle(){};
        int get_rad(){
            return _rad;
        }
        int get_x(){
            return _x;
        }
        int get_y(){
            return _y;
        }
        
        void draw(screen &scr, const pen &p) const{
            scr.set_circle(_x, _y, _rad, p.pen_to_point());
        }
        
    };
    
    class canvas {
        screen &_scr;
        std::vector< std::pair<shape*, pen> > _v;
        std::vector< std::pair<shape*, pen> >::iterator it;
        
    public:
        canvas(screen & _screen) : _scr(_screen) { }
        void add(shape* s, pen p){
            std::pair<shape*, pen> p1=std::make_pair(s, p);
            _v.push_back(p1);
        };
        void clear(){
            while (!_v.empty()) {
                _v.pop_back();
            }
            _scr.clear();
            
        }
        void show(){
            
            _scr.clear();
            for (it = _v.begin(); it != _v.end(); it++){
                std::pair<shape*, pen> c=*it;
                shape* shape_pointer=c.first;
                pen pen_ref=c.second;
                (c.first)->draw(_scr, pen_ref);
                
            }
            _scr.render();
        };
    };
}





// ############################################################
//           ! ! ! DON'T CHANGE ANYTHING BELOW ! ! !
// ############################################################

using namespace tui;

int main() {
    int cols;
    int rows;
    
    cin >> cols >> rows;
    
    screen scr(cols, rows);
    canvas canv(scr);
    
    // -- creating a few pens ------------------------------------------------
    
    pen pen_magenta(color::magenta, color::system_default, '&', true);
    pen pen_red(color::yellow, color::red, '@', true);
    pen pen_green(color::white, color::green, '$', false);
    pen pen_blue(color::blue, color::blue, '*', true);
    pen pen_white(color::black, color::white, 'O', true);
    
    // -- drawing shapes -----------------------------------------------------
    
    rectangle rect1(0, 0, scr.ncols(), scr.nrows());
    canv.add(&rect1, pen_magenta);
    
    // keeping addresses of objects created using placement new (such objects
    // need to be _explicitly_ destructed)
    vector<shape*> trash;
    
    // creating a list of pens, for convenience
    vector<pen> pens;
    pens.push_back(pen_red);
    pens.push_back(pen_green);
    pens.push_back(pen_blue);
    pens.push_back(pen_white);
    
    // filling the left part of window with nested rectangles
    int width = (scr.ncols() - 2) / 2;
    int height = scr.nrows() - 2;
    
    for(int i = 1; i <= (scr.ncols() - 2) / 8; i++) {
        shape* addr = (shape*)alloca(sizeof(rectangle));
        trash.push_back(addr);
        new (addr) rectangle(i, i, width, height);
        canv.add(addr, pens[i % pens.size()]);
        width -= 2;
        height -= 2;
        if(width <= 0 || height <= 0)
            break;
    }
    
    // filling the right part of the window with nested circles
    int x = 1 + 3 * (scr.ncols() - 2) / 4;
    int y = (scr.nrows() - 2) / 2;
    for(int rad = ((scr.ncols() - 2) >> 3) - 2; rad > 0; rad -= 2) {
        shape* addr = (shape*)alloca(sizeof(circle));
        trash.push_back(addr);
        new (addr) circle(x, y, rad);
        canv.add(addr, pens[(rad >> 1) % pens.size()]);
    }
    
    // -- updating screen -----------------------------------------------------
    
    canv.show();
    
    // -- manually destructing objects created using placement new ------------
    
    for(int i = 0; i < (int)trash.size(); i++)
        trash[i]->~shape();
    
    // !notice these objects have been allocated on the stack, so
    // there is no need to manually release the memory the objects
    // have occupied (it is done automatically). But destructors are
    // not called automatically when we use placement new.
    
    return 0;
}
