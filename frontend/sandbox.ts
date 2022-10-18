// A class for creating a sandboxed Cargo project.
export class Sandbox {
    // Contents of the user's editor.
    main_contents: string;
    // Date of request (for future logging purposes).
    requested_at: Date;

    constructor(input: string) {
        console.log("creating sandbox with contents=${input}");
        this.main_contents = input;
        this.requested_at = new Date();
    }
}
