module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({

    srcFiles: ["frontend/**/*.purs", "bower_components/**/src/**/*.purs"],

    dotPsci: ["<%=srcFiles%>"],

    psc: {
      options: {
        main: "Main",
        modules: ["Main"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "static/Main.js"
      }
    },

    watch: {
        files: ["<%=srcFiles%>"],
        tasks: ["psc:all"]
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-contrib-watch");

  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
