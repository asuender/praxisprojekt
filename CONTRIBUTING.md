# How to contribute to this project

> [!NOTE]
> This section is only relevant for you if you are directly involved here (i.e. a team member).

## General information

Although we work with aligned interests, it's always good to define some common ground as to how we write our code. This basically includes naming variables, code styling, and so on. The following outlines some guidelines:

First and foremost: let's make the process of working on this project as transparent as possible. I (Andreas) myself am a big advocator of things like issues, branching strategies etc. as _they let everyone know_ what's going on. Every team member shall be able to see how things are proceeding. Let's keep it that way.

Please **do not push any data files** (i.e. any files residing inside the `data/` directory) to GitHub! When working locally, you are free to keep them on your machine. Nevertheless, they are usually quite heavy in size to be tracked by Git. We will have scripts that download them automatically anyway.

## Workflow

1. **If you want to work on a task** (i.e. a "feature"), please **create a new branch** with an appropriate name (e.g. `presentation`, `data-loading`, etc.).

2. When you're done, **create a new pull request**. Another team member will (hopefully soon enough) look at your code and provide constructive feedback. Pushing to the main branch directly is blocked by default.

3. You will now notice that **GitHub will run automated checks** that re-format your code.

4. Once everybody is fine with the changes, go ahead and **merge into the main branch**.

Some of this will be (or already is) enforced by automatic workflows triggered on every pull request. They ensure that every new piece of code that gets integrated into the main branch adheres to the "rules" defined above.

Please don't hesitate to give feedback to other people's code - "geben und nehmen".

## Using AI

You are free to use any form of artificial intelligence (LLMs, Coding agents, etc.) to the extent that it can be justified. If you happen to use it, **please use the [AI_USAGE.md](./AI_USAGE.md) file to declare it**.
