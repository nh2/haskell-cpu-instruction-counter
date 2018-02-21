#define _GNU_SOURCE
#include <linux/perf_event.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>

#include "perfcounters.h"

// Adapted from the example in `man perf_event_open`.

/* Wrapper around the perf_event_open syscall. */
static long
perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
                int cpu, int group_fd, unsigned long flags)
{
    return syscall(__NR_perf_event_open, hw_event, pid, cpu,
                   group_fd, flags);
}

/* See docs in header file. */
int
perf_event_open_hw_instructions()
{
    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = PERF_TYPE_HARDWARE;
    pe.size = sizeof(struct perf_event_attr);
    pe.config = PERF_COUNT_HW_INSTRUCTIONS;
    pe.disabled = 1;
    pe.exclude_kernel = 1;
    pe.exclude_hv = 1;

    // Returns -1 on failure and sets errno.
    return perf_event_open(&pe, 0, -1, -1, 0);
}

/* See docs in header file. */
int
start_instruction_counter(int fd)
{
    if (ioctl(fd, PERF_EVENT_IOC_RESET, 0) == -1)
        return -1;
    if (ioctl(fd, PERF_EVENT_IOC_ENABLE, 0) == -1)
        return -1;

    return 0;
}

/* See docs in header file. */
long long
finish_instruction_counter(int fd)
{
    long long count = 0;
    if (ioctl(fd, PERF_EVENT_IOC_DISABLE, 0) == -1)
        return -1;

    ssize_t num_read = read(fd, &count, sizeof(long long));

    if (num_read != sizeof(long long))
        return -2;

    return count;
}
