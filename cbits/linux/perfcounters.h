/* Calls `perf_event_open()` with `PERF_COUNT_HW_INSTRUCTIONS`.
   Return value and `errno` is that of `perf_event_open()`.
   You should `close()` the returned FD when you're done with it.
 */
int perf_event_open_hw_instructions();

/* Starts instruction counting on the given `perf_event_open()` FD.
   Returns 0 on success.
   Returns -1 and sets `errno` on failure.
 */
int start_instruction_counter(int fd);

/* Stops instruction counting on the given `perf_event_open()` FD.
   Returns the number of instructions on success.
   Returns -1 and sets `errno` on a `perf_event_open()` related failure.
   Returns -2 on an abnormal error (if the FD didn't provide enough data).
 */
long long finish_instruction_counter(int fd);
