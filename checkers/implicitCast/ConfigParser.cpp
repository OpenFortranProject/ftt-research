#include "ConfigParser.hpp"
#include <cstdlib>
#include <algorithm>
#include <iostream>
#include <cctype>

int upper(int c)
{
  return std::toupper((unsigned char)c);
}

int lower(int c)
{
  return std::tolower((unsigned char)c);
}

ConfigParser::ConfigParser()
: space(" \t\v\n")
{}

ConfigParser::rules_t ConfigParser::parseFile(std::istream &configStream)
{
  std::string line;
  rules_t rules;
  for(getline(configStream, line); configStream; getline(configStream, line)) {
    std::string rest = dropSpaces(line);
    if( rest.size() == 0 ) continue;
    std::pair<std::string,std::string> fromType = nextType(line);
    rest = fromType.second;
    std::transform(fromType.first.begin(), fromType.first.end(), fromType.first.begin(), lower);
    //std::cout << "'" << fromType.first << "'";
    rest = dropSpaces(rest);
    if(conversionArrow(rest)) {
      rest = rest.substr(2);
    } else {
      std::cout << "Parse error: expected '->' but found '" << rest << "'" << std::endl;
      continue;
    }
    //std::cout << " -> ";
    std::pair<std::string,std::string> toType = nextType(rest);
    rest = toType.second;
    std::transform(toType.first.begin(), toType.first.end(), toType.first.begin(), lower);
    //std::cout << "'" << toType.first << "'";
    warn_e warnType(warningType(rest));
    if( warnType == eUNKNOWN ) {
      std::cout << "unrecognized warning type, expected: ok, warn, or error" << std::endl;
      continue;
    }
    //std::cout << " : '" << warn_e_ToString(warnType) << "'" << std::endl;
    cast_t cast(make_pair(fromType.first, toType.first));
    rules.insert(make_pair(cast, warnType));
  }
  return rules;
}

// Returns true if the next token is '->'
bool ConfigParser::conversionArrow(const std::string line) const
{
  if(line.size() >= 2) return line[0] == '-' && line[1] == '>';
  return false;
}

std::pair<std::string,std::string>
ConfigParser::nextType(const std::string line) const
{
  std::string ret(dropSpaces(line));
  size_t end = ret.find_first_of(space+"-:");
  return make_pair(ret.substr(0,end), ret.substr(end));
}

ConfigParser::warn_e ConfigParser::warningType(const std::string line) const
{
  std::string ret(dropSpaces(line));
  if( ret.size() >= 1 && ret[0] == ':' ) {
    ret = dropSpaces(ret.substr(1));
    ret = ret.substr(0, ret.find_first_of(space));
    std::transform(ret.begin(), ret.end(), ret.begin(), upper);
    if( ret == "WARN" )
      return eWARN;
    else if( ret == "ERROR" )
      return eERROR;
    else if( ret == "OK" )
      return eOK;
    else
      return eUNKNOWN;
  }
  return eUNKNOWN;
}

std::string warn_e_ToString(ConfigParser::warn_e warnType)
{
  switch(warnType) {
    case ConfigParser::eOK:      return "ok";
    case ConfigParser::eWARN:    return "warn";
    case ConfigParser::eERROR:   return "error";
    case ConfigParser::eUNKNOWN:
    default:                     return "<unknown>";
   }
}

std::string ConfigParser::dropSpaces(const std::string line) const
{
  // Need to deal with empty strings and strings that don't start
  // with spaces.
  if( line.size() == 0 ) return "";
  if( space.find_first_of(line[0]) == std::string::npos)
    // this means line doesn't start with a space
    return line;
  size_t pos = line.find_first_not_of(space);
  if( pos == std::string::npos ) return line;
  return line.substr(pos);
}
