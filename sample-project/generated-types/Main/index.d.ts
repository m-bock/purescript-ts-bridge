import * as SampleApp_Types from "~/SampleApp.Types";

export type Foo = number;

export type Bar = (_: number) => (_: string) => SampleApp_Types.AppState;
