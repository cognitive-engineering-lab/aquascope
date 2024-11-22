class Debug {
  prefix?: string = "wasi:";
  log: (...args: unknown[]) => void;

  constructor(private isEnabled: boolean) {
    this.log = (..._args: any) => {};
    this.enable(isEnabled);
  }

  // Recreate the logger function with the new enabled state.
  enable(enabled?: boolean) {
    this.log = createLogger(
      enabled === undefined ? true : enabled,
      this.prefix
    );
  }

  // Getter for the private isEnabled property.
  get enabled(): boolean {
    return this.isEnabled;
  }
}

// The createLogger() creates either an empty function or a bound console.log
// function so we can retain accurate line lumbers on Debug.log() calls.
function createLogger(
  enabled: boolean,
  prefix?: string
): (...args: unknown[]) => void {
  if (enabled) {
    const a = console.log.bind(console, "%c%s", "color: #265BA0", prefix);
    return a;
  } else {
    return () => {};
  }
}

export const debug = new Debug(false);
