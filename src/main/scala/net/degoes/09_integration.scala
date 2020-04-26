package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, different domains must often be combined into a common
 * domain. This is done by translating / interpreting from these domains into
 * another domain that is general enough and powerful enough to solve both
 * problems.
 *
 * In the larger, more powerful domain, solutions are less constrained, which
 * provides fewer benefits around reasoning, testing, and rich features.
 *
 * In practice, when different domains need to be combined, they will often be
 * combined into an effect domain such as ZIO.
 *
 * In this section, you'll gain experience translating related problems into
 * a common domain.
 */

object multiple_domains {}
