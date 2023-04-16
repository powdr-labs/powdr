/* Memory layout is assumed to be 1GB of RAM */
MEMORY
{
  RAM : ORIGIN = 0x00000000, LENGTH = 1024M
}

/* The entry point is the reset handler */
ENTRY(start);

SECTIONS
{
  .start ORIGIN(RAM) :
  {
    KEEP(*(.start));  
  } > RAM

  .text :
  {
    *(.text .text.* .eh_*);
  } > RAM

  /* TODO : discard all extra sections */
  /DISCARD/ :
  {
    *(.ARM.exidx .ARM.exidx.*);
  }
}
