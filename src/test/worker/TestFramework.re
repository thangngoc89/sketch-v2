include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "path/to/test/lib/__snapshots__",
      projectDir: "/home/khoa/web/revery-playground/test"
    });
});
