#!/usr/bin/env node
import { main } from "../output/TsBridgeGen.Cli/index.js";

process.env["ASSETS_DIR"] = `${__dirname}/../assets`; 

main();