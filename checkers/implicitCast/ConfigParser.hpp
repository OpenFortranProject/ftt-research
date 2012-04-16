#ifndef CONFIGPARSER_HPP
#define CONFIGPARSER_HPP

#include <iostream>
#include <vector>
#include <string>
#include <map>

class ConfigParser {
public:
  ConfigParser();

  enum warn_e { eUNKNOWN, eOK, eWARN, eERROR };
  enum linetype_e { eCAST, eALIAS };
  typedef std::pair<std::string,std::string> cast_t;
  typedef std::map<cast_t, warn_e> rules_t;
  typedef std::map<std::string, std::string> aliases_t;
  std::pair<rules_t, aliases_t> parseFile(std::istream &configStream);

private:
  std::pair<std::string,std::string> nextType(const std::string line) const;
  std::string dropSpaces(const std::string line) const;
  bool conversionArrow(const std::string line) const;
  bool equality(const std::string line) const;
  warn_e warningType(const std::string line) const;


  const std::string space;
};

std::string warn_e_ToString(ConfigParser::warn_e warnType);
int upper(int c);
int lower(int c);
int parseKind(const std::string type);

#endif /* CONFIGPARSER_HPP */
