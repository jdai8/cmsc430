all:
	clang++ --std=c++11 -pthread header.cpp -S -emit-llvm -o header.ll

clean:
	rm *.ll
