context("batch class - initialization")

test_that("inexisting path generate error", {
    fake_file <- paste(letters, collapse = "")
    expect_false(fake_file %in% list.files())
    expect_error(new(Class = "batch", path = fake_file), "Filepath must exists")
})

test_that("unexecutable filepath generate error", {
    if (Sys.info()["sysname"] != "Windows") {
        # Create file without execute permission
        bad_file <- "./bad_file"
        file.create(bad_file)
        Sys.chmod(bad_file, mode = "0600", use_umask = FALSE)
        expect_equal(file.access(bad_file, 1), -1)
        expect_error(new(Class = "batch", path = bad_file), "File must be executable")
    }
})

good_file <- "./good_file.R"
file.create(good_file)
Sys.chmod(good_file, mode = "0770", use_umask = FALSE)

test_that("Param not null but not list generate error", {
    expect_error(new(Class = "batch", path = good_file, params = c(1:10)), "If list specified then must be a list")
})

test_that("Param list but missing one name generate error", {
    expect_error(new(Class = "batch", path = good_file, params = list(1, 1:10, "hello")), "All slots in params must be named")
})

test_that("parallelizable not logical returns error", {
    expect_error(new(Class = "batch", path = good_file, parallelizable = "hello"))
})

test_that("structure object", {
    parallelizable <- FALSE
    params <- list(w = 1, x = "hello world", y = c(1:10), z = iris)
    obj <- new(Class = "batch", path = good_file, params = params, parallelizable = FALSE)
    expect_is(obj, "batch")
    expect_equal(obj@alias, tools::file_path_sans_ext(basename(good_file)))
    expect_equal(obj@path, normalizePath(good_file))
    expect_equal(obj@parallelizable, parallelizable)
    expect_identical(obj@params, params)
    expect_true(all(c("alias", "path", "params", "parallelizable") %in% slotNames(obj)))
})

# Clean files & env
unlink(bad_file, good_file)
rm(fake_file, bad_file, good_file, params, obj, parallelizable)
