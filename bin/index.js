#!/usr/bin/env node
import { main } from "../output/TsBridgeGen.Main/index.js";
import url from "url";

process.env["ASSETS_DIR"] = url.fileURLToPath(new URL('../assets', import.meta.url)); 

main();