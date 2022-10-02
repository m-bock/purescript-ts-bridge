#!/usr/bin/env node
import { main } from "../output/TsBridgeGen.Main/index.js";

process.env["ASSETS_DIR"] = `${__dirname}/../assets`; 

main();