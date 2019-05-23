// Copyright (c) 2000 Mike Morearty <mike@morearty.com>
// Original source and docs: http://www.morearty.com/code/breakpoint

#ifndef _HARDWAREBP_H_
#define _HARDWAREBP_H_

#ifdef _DEBUG

// The enum values correspond to the values used by the Intel Pentium,
// so don't change them!
enum HardwareBreakpoint_Condition {
    HWBP_WRITE = 1,
    HWBP_READ = 3
};

inline void SetBits(unsigned long* dw, int lowBit, int bits, int newValue)
{
    int mask = (1 << bits) - 1; // e.g. 1 becomes 0001, 2 becomes 0011, 3 becomes 0111

    *dw = (*dw & ~(mask << lowBit)) | (newValue << lowBit);
}

int HardwareBreakpoint_Set(void* address, int len /* 1, 2, or 4 */, int when);
void HardwareBreakpoint_Clear(int m_index);

#endif // _DEBUG

#endif // _HARDWAREBP_H_
